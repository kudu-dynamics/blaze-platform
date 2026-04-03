{-# LANGUAGE CPP #-}

module Main where

import Flint.Prelude

import Flint.App (withBackend, Backend)
import Flint.Cfg.Path (enableSamplingTiming)
import qualified Flint.Cfg.Store as Store
import Data.IORef
import Flint.Shell.Types (ShellState, CommandResult(..), initShellState)
import Flint.Shell.Command (dispatchCommand)
import Flint.Shell.Repl (allCommands)

import Blaze.Import.Binary (getBase, inspectAddress, saveToDb)

import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Directory (doesFileExist)
import qualified System.IO as SIO
import Options.Applicative hiding (info)
import qualified Options.Applicative as OA

import MCP.Server (runMcpServerStdio, runMcpServerHttpWithConfig, McpServerInfo(..), McpServerHandlers(..), Content(..), Error(..))
import MCP.Server.Transport.Http (HttpConfig(..))
import MCP.Server.Types
  ( ToolDefinition(..)
  , InputSchemaDefinition(..)
  , InputSchemaDefinitionProperty(..)
  )


data McpOptions = McpOptions
  { backend        :: Maybe Backend
  , doNotUseSolver :: Bool
  , profileSampling :: Bool
  , analysisDb     :: Maybe FilePath
  , typeHintsFile  :: Maybe FilePath
  , useHttp        :: Bool
  , httpPort       :: Int
  }
  deriving (Eq, Ord, Read, Show, Generic)

-- | Mutable state for the MCP server, allowing dynamic binary loading.
data McpState = McpState
  { shellStateRef    :: IORef (Maybe ShellState)
  , binaryShutdown   :: IORef (Maybe (MVar ()))
  , options          :: McpOptions
  }
  deriving (Generic)

parseBackend :: Parser Backend
parseBackend = option auto $
  long "backend"
  <> metavar "BACKEND"
  <> help
     ( "preferred backend ("
#ifdef FLINT_SUPPORT_BINARYNINJA
         <> "BinaryNinja or "
#endif
         <> "Ghidra)"
     )

parseAnalysisDb :: Parser FilePath
parseAnalysisDb = strOption $
  long "analysisDb"
  <> metavar "ANALYSIS_DB"
  <> help "DB to save and load analysis data"

parseDoNotUseSolver :: Parser Bool
parseDoNotUseSolver = switch $
  long "doNotUseSolver"
  <> help "do not enable the SMT solver by default"

parseProfileSampling :: Parser Bool
parseProfileSampling = switch $
  long "profileSampling"
  <> help "print timing info for path sampling phases to stderr"

parseTypeHintsFile :: Parser FilePath
parseTypeHintsFile = strOption $
  long "typeHints"
  <> short 't'
  <> metavar "TYPEHINT_FILE"
  <> help "file containing functions that we should get type hints for"

parseUseHttp :: Parser Bool
parseUseHttp = switch $
  long "http"
  <> help "Use HTTP transport instead of stdio"

parseHttpPort :: Parser Int
parseHttpPort = option auto $
  long "port"
  <> metavar "PORT"
  <> value 3000
  <> help "Port for HTTP transport (default: 3000)"

optionsParser :: Parser McpOptions
optionsParser = McpOptions
  <$> optional parseBackend
  <*> (parseDoNotUseSolver <|> pure False)
  <*> (parseProfileSampling <|> pure False)
  <*> optional parseAnalysisDb
  <*> optional parseTypeHintsFile
  <*> parseUseHttp
  <*> parseHttpPort

main :: IO ()
main = do
  opts <- execParser optsParser
  when (opts ^. #profileSampling) enableSamplingTiming
  stateRef <- newIORef Nothing
  shutdownRef <- newIORef Nothing
  let mcpSt = McpState
        { shellStateRef = stateRef
        , binaryShutdown = shutdownRef
        , options = opts
        }
  if opts ^. #useHttp
    then do
      let port = opts ^. #httpPort
      SIO.hPutStrLn stderr $ "Starting MCP HTTP server on port " <> show port <> " (no binary loaded)..."
      runMcpServerHttpWithConfig
        (HttpConfig { httpPort = port, httpHost = "localhost", httpEndpoint = "/mcp", httpVerbose = False })
        serverInfo
        (handlers mcpSt)
    else do
      SIO.hPutStrLn stderr "Starting MCP server (stdio). Use load_binary to load a binary."
      runMcpServerStdio serverInfo (handlers mcpSt)
  where
    optsParser = OA.info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "MCP server exposing Flint binary analysis tools."
     <> header "flint-mcp" )

getFuncsFromFile :: FilePath -> IO (HashSet Text)
getFuncsFromFile fp
  = HashSet.fromList
  . filter (not . Text.null)
  . fmap Text.strip
  . Text.lines
  <$> liftIO (TextIO.readFile fp)


-- | MCP server metadata
serverInfo :: McpServerInfo
serverInfo = McpServerInfo
  { serverName = "flint-mcp"
  , serverVersion = "0.1.0"
  , serverInstructions = Text.unlines
      [ "Flint is a binary vulnerability detection tool."
      , "Use 'load_binary' to load a binary file for analysis."
      , "Use 'list_functions' to discover functions in the loaded binary."
      , "Use 'calls' to find call sites to a function (internal or extern, e.g. system, popen)."
      , "Use 'sample_paths' to sample execution paths from a function (reduced by default)."
      , "Use 'show_paths' to view the PIL statements on a sampled path (use N! for raw/unreduced)."
      , "Use 'check_wmi' to check if a path matches a vulnerability pattern."
      , "Use 'list_wmis' to see all available vulnerability patterns."
      , "Use 'input_detect' to auto-discover common input sources (recv, read, fgets, etc.) in the binary."
      , "Use 'input_genesis' to sample paths from a function with interprocedural call expansion."
      , "Typical workflow: load_binary -> list_functions -> sample_paths -> show_paths -> check_wmi"
      , "Input genesis workflow: load_binary -> input_detect -> input_genesis -> show_paths -> check_wmi"
      ]
  }


-- | MCP handlers: only tools, no prompts or resources
handlers :: McpState -> McpServerHandlers IO
handlers mcpSt = McpServerHandlers
  { prompts   = Nothing
  , resources = Nothing
  , tools     = Just (pure toolDefinitions, handleToolCall mcpSt)
  }


-- | Convert a CommandResult to text for MCP response
renderResultText :: CommandResult -> Text
renderResultText = \case
  ResultText t -> t
  ResultOk t -> t
  ResultError t -> "Error: " <> t
  ResultFunctions internals externs ->
    let internalLines = if null internals then []
          else "Internal Functions:"
             : fmap (\(name, addr) -> "  " <> show addr <> "  " <> name) internals
        externLines = if null externs then []
          else "External Functions:"
             : fmap (\(name, mLib) -> "  " <> name <> maybe "" (\l -> " (" <> l <> ")") mLib) externs
        sep = if not (null internals) && not (null externs) then [""] else []
    in Text.unlines $ internalLines <> sep <> externLines
  ResultTextAndPaths header' paths -> header' <> "\n" <>
    Text.unlines (fmap (\(pid, summary) -> "[" <> show pid <> "] " <> summary) paths)
  ResultPaths paths ->
    Text.unlines $ fmap (\(pid, summary) -> "[" <> show pid <> "] " <> summary) paths
  ResultSolver results ->
    Text.unlines $ fmap (\(pid, res) -> "[" <> show pid <> "] " <> res) results
  ResultWMIs results ->
    Text.unlines $ concatMap (\(pid, msgs) ->
      ("[" <> show pid <> "]") : msgs) results


-- | Load a binary file, resetting all state. The importer is kept alive in a
--   background thread so that lazy CFG computations can still access it.
loadBinary :: McpState -> FilePath -> IO (Either Error Content)
loadBinary mcpSt fp = do
  exists <- doesFileExist fp
  if not exists
    then pure $ Left $ InvalidParams $ "File not found: " <> cs fp
    else do
      -- Shutdown previous binary if loaded
      mOldShutdown <- readIORef (mcpSt ^. #binaryShutdown)
      forM_ mOldShutdown $ \mv -> void $ tryPutMVar mv ()
      writeIORef (mcpSt ^. #shellStateRef) Nothing
      writeIORef (mcpSt ^. #binaryShutdown) Nothing

      let opts = mcpSt ^. #options
      readyMVar <- newEmptyMVar :: IO (MVar (Either Text ()))
      shutdownMVar <- newEmptyMVar :: IO (MVar ())

      SIO.hPutStrLn stderr $ "Loading " <> fp <> "..."
      setVerbosity VDebug

      _ <- forkIO $
        withBackend (opts ^. #backend) fp (\imp -> do
          typeHintsWhitelist <- maybe (pure HashSet.empty) getFuncsFromFile (opts ^. #typeHintsFile)
          analysisDbPath <- Store.resolveAnalysisDb (opts ^. #analysisDb) fp
          (store, _) <- Store.initWithTypeHints typeHintsWhitelist HashSet.empty analysisDbPath imp
          base <- getBase imp
          st <- initShellState store base (not $ opts ^. #doNotUseSolver) (Just $ inspectAddress imp) (Just $ \outPath -> saveToDb outPath imp)
          writeIORef (mcpSt ^. #shellStateRef) (Just st)
          SIO.hPutStrLn stderr $ "Binary loaded: " <> fp
          putMVar readyMVar (Right ())
          -- Block until told to shutdown (keeps importer alive for lazy CFG access)
          takeMVar shutdownMVar
        ) `catch` \(e :: SomeException) ->
          void $ tryPutMVar readyMVar (Left $ show e)

      result <- takeMVar readyMVar
      case result of
        Left err -> pure $ Left $ InternalError $ "Failed to load binary: " <> err
        Right () -> do
          writeIORef (mcpSt ^. #binaryShutdown) (Just shutdownMVar)
          pure $ Right $ ContentText $ "Binary loaded: " <> cs fp


-- | Get the current ShellState, or return an error if no binary is loaded.
requireBinary :: McpState -> IO (Either Error ShellState)
requireBinary mcpSt = do
  mSt <- readIORef (mcpSt ^. #shellStateRef)
  case mSt of
    Nothing -> pure $ Left $ InternalError "No binary loaded. Use load_binary first."
    Just st -> pure $ Right st


-- | Dispatch an MCP tool call by building a shell command string
handleToolCall :: McpState -> Text -> [(Text, Text)] -> IO (Either Error Content)
handleToolCall _mcpSt "exit" _args = do
  SIO.hPutStrLn stderr "Shutting down flint-mcp..."
  exitSuccess
handleToolCall mcpSt "load_binary" args =
  case lookupArg "file_path" args of
    Nothing -> pure $ Left $ InvalidParams "Missing required parameter: file_path"
    Just fp -> loadBinary mcpSt (cs fp)
handleToolCall mcpSt "set_solver" args =
  case lookupArg "enabled" args of
    Nothing -> pure $ Left $ InvalidParams "Missing required parameter: enabled"
    Just val -> do
      eSt <- requireBinary mcpSt
      case eSt of
        Left err -> pure $ Left err
        Right st
          | val `elem` ["true", "1", "on"] -> do
              writeIORef (st ^. #useSolver) True
              pure $ Right $ ContentText "Solver enabled."
          | val `elem` ["false", "0", "off"] -> do
              writeIORef (st ^. #useSolver) False
              pure $ Right $ ContentText "Solver disabled."
          | otherwise -> pure $ Left $ InvalidParams $ "Invalid value for enabled: " <> val
handleToolCall mcpSt toolName args = do
  eSt <- requireBinary mcpSt
  case eSt of
    Left err -> pure $ Left err
    Right st -> do
      let cmdStr = buildCommandString toolName args
      case cmdStr of
        Left err -> pure $ Left $ InvalidParams err
        Right cmd -> do
          result <- catch
            (dispatchCommand allCommands st cmd)
            (\(e :: SomeException) -> pure $ ResultError $ "Error: " <> show e)
          case result of
            ResultError msg -> pure $ Left $ InternalError msg
            other -> pure $ Right $ ContentText $ renderResultText other


-- | Build a shell command string from MCP tool name + arguments
buildCommandString :: Text -> [(Text, Text)] -> Either Text Text
buildCommandString toolName args = case toolName of
  "list_functions" ->
    let filterArg = lookupArg "filter" args
    in Right $ "functions" <> maybe "" (" " <>) filterArg

  "sample_paths" ->
    case lookupArg "function" args of
      Nothing -> Left "Missing required parameter: function"
      Just func ->
        let count = lookupArg "count" args
            addrs = lookupArg "addresses" args
            depthPart = maybe "" (" --depth " <>) (lookupArg "depth" args)
            countPart = maybe "" (" " <>) count
            addrPart = maybe "" (" @ " <>) addrs
            unrollPart = case lookupArg "unroll_loops" args of
              Just "true" -> " --unrollLoops"
              _ -> ""
        in Right $ "sample" <> countPart <> " " <> func <> addrPart <> depthPart <> unrollPart

  "show_paths" ->
    case lookupArg "path_ids" args of
      Nothing -> Left "Missing required parameter: path_ids"
      Just pids -> Right $ "show " <> pids

  "pshow_path" ->
    case lookupArg "path_id" args of
      Nothing -> Left "Missing required parameter: path_id"
      Just pid ->
        let addrs = lookupArg "addresses" args
        in Right $ "pshow " <> pid <> maybe "" (" " <>) addrs

  "free_paths" ->
    case lookupArg "path_ids" args of
      Nothing -> Left "Missing required parameter: path_ids"
      Just pids -> Right $ "free " <> pids

  "list_paths" -> Right "paths"

  "list_wmis" -> Right "wmis"

  "check_wmi" ->
    case (lookupArg "wmi_name" args, lookupArg "path_ids" args) of
      (Nothing, _) -> Left "Missing required parameter: wmi_name"
      (_, Nothing) -> Left "Missing required parameter: path_ids"
      (Just wmi, Just pids) -> Right $ "check-wmi " <> wmi <> " " <> pids

  "solve_paths" ->
    case lookupArg "path_ids" args of
      Nothing -> Left "Missing required parameter: path_ids"
      Just pids -> Right $ "solve " <> pids

  "tag_path" ->
    case (lookupArg "path_id" args, lookupArg "name" args) of
      (Nothing, _) -> Left "Missing required parameter: path_id"
      (_, Nothing) -> Left "Missing required parameter: name"
      (Just pid, Just name) -> Right $ "tag " <> pid <> " " <> name

  "free_untagged" -> Right "free-untagged"

  "expand_call" ->
    case (lookupArg "path_id" args, lookupArg "address" args) of
      (Nothing, _) -> Left "Missing required parameter: path_id"
      (_, Nothing) -> Left "Missing required parameter: address"
      (Just pid, Just addr) ->
        let pathsPart = case lookupArg "paths" args of
              Just ps -> " --paths " <> ps
              Nothing -> case lookupArg "count" args of
                Just c  -> " " <> c
                Nothing -> ""
        in Right $ "expand " <> pid <> " " <> addr <> pathsPart

  "calls" ->
    case lookupArg "func_name" args of
      Nothing -> Left "Missing required parameter: func_name"
      Just name -> Right $ "calls " <> name

  "strings" ->
    let filterArg = lookupArg "filter" args
    in Right $ "strings" <> maybe "" (" " <>) filterArg

  "string_xrefs" ->
    case lookupArg "target" args of
      Nothing -> Left "Missing required parameter: target"
      Just target -> Right $ "string-xrefs " <> target

  "input_genesis" ->
    let countPart = maybe "" (" " <>) (lookupArg "count" args)
        depthPart = maybe "" (" --depth " <>) (lookupArg "depth" args)
    in Right $ "input-genesis" <> countPart <> depthPart

  "typecheck" ->
    case lookupArg "path_ids" args of
      Nothing -> Left "Missing required parameter: path_ids"
      Just pids -> Right $ "typecheck " <> pids

  "inspect_address" ->
    case lookupArg "address" args of
      Nothing -> Left "Missing required parameter: address"
      Just addr -> Right $ "inspect " <> addr

  "save_binary" ->
    case lookupArg "file_path" args of
      Nothing -> Left "Missing required parameter: file_path"
      Just fp -> Right $ "save " <> fp

  -- prim-def commands
  "prim_def" ->
    case lookupArg "definition" args of
      Nothing -> Left "Missing required parameter: definition"
      Just def' -> Right $ "prim-def " <> def'

  "prim_list" -> Right "prim-list"

  "prim_remove" ->
    case lookupArg "name" args of
      Nothing -> Left "Missing required parameter: name"
      Just name -> Right $ "prim-remove " <> name

  "prim_reset" -> Right "prim-reset"

  -- taint commands
  "taint_add" ->
    case lookupArg "command" args of
      Nothing -> Left "Missing required parameter: command"
      Just cmd -> Right $ "taint-add " <> cmd

  "taint_list" -> Right "taint-list"

  "taint_remove" ->
    case lookupArg "function" args of
      Nothing -> Left "Missing required parameter: function"
      Just func -> Right $ "taint-remove " <> func

  "taint_reset" -> Right "taint-reset"

  -- stdlib commands
  "stdlib_add" ->
    case lookupArg "command" args of
      Nothing -> Left "Missing required parameter: command"
      Just cmd -> Right $ "stdlib-add " <> cmd

  "stdlib_list" -> Right "stdlib-list"

  "stdlib_remove" ->
    case lookupArg "function" args of
      Nothing -> Left "Missing required parameter: function"
      Just func -> Right $ "stdlib-remove " <> func

  "analyze_all" -> Right "analyze-all"

  -- These are handled directly in handleToolCall, not via command dispatch
  "set_solver" -> Left "handled_directly"
  "exit" -> Left "handled_directly"
  "load_binary" -> Left "handled_directly"

  _ -> Left $ "Unknown tool: " <> toolName


lookupArg :: Text -> [(Text, Text)] -> Maybe Text
lookupArg key = fmap snd . find (\(k, _) -> k == key)


-- | MCP tool definitions
toolDefinitions :: [ToolDefinition]
toolDefinitions =
  [ ToolDefinition
      { toolDefinitionName = "load_binary"
      , toolDefinitionDescription = "Load a binary file for analysis. This resets all state (cached paths, results, etc). Must be called before using any other tool."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("file_path", InputSchemaDefinitionProperty "string" "Path to the binary file (e.g. .gzf)")
              ]
          , required = ["file_path"]
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "list_functions"
      , toolDefinitionDescription = "List functions in the binary. Shows both internal and external (imported) functions. Use -i for internal only, -e for extern only. Optionally filter by substring."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("filter", InputSchemaDefinitionProperty "string" "Optional substring to filter function names. Prefix with -i or -e to show only internal or external functions (e.g. '-e printf').")
              ]
          , required = []
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "calls"
      , toolDefinitionDescription = "Find functions that call a given function (internal or extern). Returns call sites as 'FuncName @ 0xAddress' which can be used with sample_paths addresses parameter."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("func_name", InputSchemaDefinitionProperty "string" "Name of the function to find callers of (e.g. 'system', 'popen', 'sprintf', or an internal function name)")
              ]
          , required = ["func_name"]
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "sample_paths"
      , toolDefinitionDescription = "Sample execution paths from a function. Paths are reduced (copy/constant propagation) by default and cached by path ID. Loops are summarized into a single abstract iteration by default; use unroll_loops to get the old unrolling behavior for comparison. Use --depth to auto-expand internal calls interprocedurally."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("function", InputSchemaDefinitionProperty "string" "Function name or hex address (e.g. 'main' or '0x401000')")
              , ("count", InputSchemaDefinitionProperty "string" "Number of paths to sample (optional)")
              , ("addresses", InputSchemaDefinitionProperty "string" "Space-separated hex addresses that paths must pass through (optional)")
              , ("depth", InputSchemaDefinitionProperty "string" "Auto-expand internal calls N levels deep (optional, e.g. '2')")
              , ("unroll_loops", InputSchemaDefinitionProperty "string" "Set to 'true' to use old loop unrolling instead of loop summarization (optional)")
              ]
          , required = ["function"]
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "show_paths"
      , toolDefinitionDescription = "Show PIL (Platform Independent Language) statements for cached paths. Shows reduced view by default; use N! suffix for raw/unreduced (e.g. '0!' for raw path 0)."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("path_ids", InputSchemaDefinitionProperty "string" "Path IDs to show (e.g. '0 1 2', '0..5', '[0,1,2]', '0!' for raw)")
              ]
          , required = ["path_ids"]
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "pshow_path"
      , toolDefinitionDescription = "Show raw Haskell PIL types for a path's statements. Useful for debugging."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("path_id", InputSchemaDefinitionProperty "string" "Path ID to show")
              , ("addresses", InputSchemaDefinitionProperty "string" "Optional hex addresses to filter statements (space-separated)")
              ]
          , required = ["path_id"]
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "free_paths"
      , toolDefinitionDescription = "Free cached paths to release memory."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("path_ids", InputSchemaDefinitionProperty "string" "Path IDs to free (e.g. '0 1 2', '0..5')")
              ]
          , required = ["path_ids"]
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "list_paths"
      , toolDefinitionDescription = "List all currently cached paths with their metadata."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties = []
          , required = []
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "list_wmis"
      , toolDefinitionDescription = "List all available WMI (Weak Memory Integrity) vulnerability primitives that can be checked against paths."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties = []
          , required = []
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "check_wmi"
      , toolDefinitionDescription = "Check paths against a WMI vulnerability primitive. Use 'all' as wmi_name to check all primitives at once. Returns variable mappings and locations if a match is found."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("wmi_name", InputSchemaDefinitionProperty "string" "Name of the WMI primitive, or 'all' to check all (use list_wmis to see available)")
              , ("path_ids", InputSchemaDefinitionProperty "string" "Path IDs to check (e.g. '0 1 2', '0..5')")
              ]
          , required = ["wmi_name", "path_ids"]
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "solve_paths"
      , toolDefinitionDescription = "Check path satisfiability using the Z3 SMT solver. Returns SAT/UNSAT with variable bindings."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("path_ids", InputSchemaDefinitionProperty "string" "Path IDs to solve (e.g. '0 1 2', '0..5')")
              ]
          , required = ["path_ids"]
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "tag_path"
      , toolDefinitionDescription = "Tag a cached path with a human-readable name. Tagged names can be used in place of numeric path IDs in all commands."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("path_id", InputSchemaDefinitionProperty "string" "Path ID or existing tag name to tag")
              , ("name", InputSchemaDefinitionProperty "string" "Tag name to assign (e.g. 'sprintf_system_all4')")
              ]
          , required = ["path_id", "name"]
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "free_untagged"
      , toolDefinitionDescription = "Free all untagged cached paths. Keeps paths that have been tagged with 'tag_path'."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties = []
          , required = []
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "expand_call"
      , toolDefinitionDescription = "Expand a callsite in a cached path with callee paths, creating interprocedural paths. Either sample fresh paths through the callee (count) or stitch in specific cached paths (paths). Each expansion produces a new cached path."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("path_id", InputSchemaDefinitionProperty "string" "Path ID containing the callsite to expand")
              , ("address", InputSchemaDefinitionProperty "string" "Hex address of the call instruction (e.g. '0x431458')")
              , ("count", InputSchemaDefinitionProperty "string" "Number of fresh callee paths to sample (default 1). Mutually exclusive with 'paths'.")
              , ("paths", InputSchemaDefinitionProperty "string" "Space-separated path IDs of existing callee paths to stitch in. Mutually exclusive with 'count'.")
              ]
          , required = ["path_id", "address"]
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "strings"
      , toolDefinitionDescription = "List strings in the binary. No argument lists all strings. A quoted string filters by content (case-insensitive). A bare hex address looks up a specific string."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("filter", InputSchemaDefinitionProperty "string" "Quoted substring to filter (e.g. '\"iptables\"'), or bare hex address to look up (e.g. '0x475648')")
              ]
          , required = []
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "string_xrefs"
      , toolDefinitionDescription = "Find functions that reference a string. A bare hex address shows xrefs for that specific string. A quoted string filters all strings by content and shows xrefs for each match, grouped by string."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("target", InputSchemaDefinitionProperty "string" "Quoted substring to search (e.g. '\"iptables\"'), or bare hex address of a string (e.g. '0x475648')")
              ]
          , required = ["target"]
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "set_solver"
      , toolDefinitionDescription = "Enable or disable the Z3 SMT solver for WMI checking and path solving."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("enabled", InputSchemaDefinitionProperty "string" "Set to 'true' or 'false'")
              ]
          , required = ["enabled"]
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "exit"
      , toolDefinitionDescription = "Shut down the flint-mcp server."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties = []
          , required = []
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "input_genesis"
      , toolDefinitionDescription = "Auto-detect common input source functions (recv, read, fgets, scanf, getenv, etc.) in the binary, find all callers, and sample paths through each call site. Reveals how attacker-controlled data flows from input sources through the program."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("count", InputSchemaDefinitionProperty "string" "Number of paths to sample per call site (default 5)")
              , ("depth", InputSchemaDefinitionProperty "string" "Auto-expand internal calls N levels deep (default 0, try 2 for dispatcher/handler patterns)")
              ]
          , required = []
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "typecheck"
      , toolDefinitionDescription = "Type-check paths using the PIL type checker (ConstraintGen + Unify). Shows inferred types for each expression and reports how long type checking took. Useful for profiling type checker performance on paths with complex expressions."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("path_ids", InputSchemaDefinitionProperty "string" "Path IDs to type-check (e.g. '0 1 2', '0..5')")
              ]
          , required = ["path_ids"]
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "inspect_address"
      , toolDefinitionDescription = "Inspect the raw instruction and P-code at a given address. Shows the assembly instruction, which Ghidra basic block contains it, and the raw P-code operations. Useful for understanding how binary addresses map to the PIL IR."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("address", InputSchemaDefinitionProperty "string" "Hex address to inspect (e.g. '0x804d509')")
              ]
          , required = ["address"]
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "save_binary"
      , toolDefinitionDescription = "Save the currently loaded binary as a .gzf file. This is a serialized Ghidra database that can be loaded later without needing the original binary or Ghidra analysis."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("file_path", InputSchemaDefinitionProperty "string" "Output file path (e.g. '/tmp/binary.gzf'). A .gzf extension is added if not present.")
              ]
          , required = ["file_path"]
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "analyze_all"
      , toolDefinitionDescription = "Pre-analyze all internal functions: decompile and build CFGs for every function. Results are persisted to the analysis DB (.flintdb) so future sessions load instantly. This can take a while for large binaries."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties = []
          , required = []
          }
      , toolDefinitionTitle = Nothing
      }
  -- prim-def commands
  , ToolDefinition
      { toolDefinitionName = "prim_def"
      , toolDefinitionDescription = "Define a custom WMI vulnerability primitive using the prim-def DSL. The pattern can then be checked against paths with check_wmi. Syntax: 'Name { stmt ; stmt ; ... }' where statements can be: '* ' (wildcard), 'label: ...' (labeled), 'def dst = call func(args)', 'call func(args)', 'store dst val', 'ret expr', 'constraint expr'. Use '@name' to reuse bindings, 'name[tainted]' for taint checks."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("definition", InputSchemaDefinitionProperty "string" "Full prim-def definition, e.g. 'MyPrim { * ; alloc: def p = call malloc(sz) ; * ; copy: call memcpy(@p, src, len) }'")
              ]
          , required = ["definition"]
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "prim_list"
      , toolDefinitionDescription = "List all user-defined custom WMI primitives (defined via prim_def)."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties = []
          , required = []
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "prim_remove"
      , toolDefinitionDescription = "Remove a user-defined WMI primitive by name."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("name", InputSchemaDefinitionProperty "string" "Name of the primitive to remove")
              ]
          , required = ["name"]
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "prim_reset"
      , toolDefinitionDescription = "Remove all user-defined WMI primitives."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties = []
          , required = []
          }
      , toolDefinitionTitle = Nothing
      }
  -- taint commands
  , ToolDefinition
      { toolDefinitionName = "taint_add"
      , toolDefinitionDescription = "Add a custom taint propagation rule for a function. Tells Flint that the function propagates attacker-controlled data from a source to a destination. Parameter forms: 'arg:N', 'src:N', 'dst:N', 'ret', 'other:<name>'."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("command", InputSchemaDefinitionProperty "string" "Taint rule: '<func_name> <from> <to>', e.g. 'custom_read src:0 ret' or 'recv_packet src:0 ret'")
              ]
          , required = ["command"]
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "taint_list"
      , toolDefinitionDescription = "List all custom taint propagation rules."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties = []
          , required = []
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "taint_remove"
      , toolDefinitionDescription = "Remove a custom taint propagation rule by function name."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("function", InputSchemaDefinitionProperty "string" "Name of the function to remove taint rule for")
              ]
          , required = ["function"]
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "taint_reset"
      , toolDefinitionDescription = "Remove all custom taint propagation rules."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties = []
          , required = []
          }
      , toolDefinitionTitle = Nothing
      }
  -- stdlib commands
  , ToolDefinition
      { toolDefinitionName = "stdlib_add"
      , toolDefinitionDescription = "Register a function as a known semantic primitive. Use when a custom function is a wrapper around a standard operation (allocator, copy, free, etc.). This is preferred over prim_def for simple wrappers."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("command", InputSchemaDefinitionProperty "string" "Registration: '<func> <primType> <var>=<mapping> ...', e.g. 'my_alloc allocHeap ptr=ret size=arg:0' or 'my_copy copyMem dest_ptr=arg:0 src_ptr=arg:1 len=arg:2'")
              ]
          , required = ["command"]
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "stdlib_list"
      , toolDefinitionDescription = "List all registered known function semantic mappings (both built-in and user-added via stdlib_add)."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties = []
          , required = []
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "stdlib_remove"
      , toolDefinitionDescription = "Remove a user-registered known function mapping by function name."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("function", InputSchemaDefinitionProperty "string" "Name of the function to remove")
              ]
          , required = ["function"]
          }
      , toolDefinitionTitle = Nothing
      }
  ]
