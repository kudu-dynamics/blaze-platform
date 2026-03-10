{-# LANGUAGE CPP #-}

module Main where

import Flint.Prelude

import Flint.App (withBackend, Backend)
import qualified Flint.Cfg.Store as Store
import Data.IORef
import Flint.Shell.Types (ShellState, CommandResult(..), initShellState)
import Flint.Shell.Command (dispatchCommand)
import Flint.Shell.Repl (allCommands)

import Blaze.Import.Binary (getBase)

import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Directory (doesFileExist)
import qualified System.IO as SIO
import Options.Applicative hiding (info)
import qualified Options.Applicative as OA

import MCP.Server (runMcpServerStdio, McpServerInfo(..), McpServerHandlers(..), Content(..), Error(..))
import MCP.Server.Types
  ( ToolDefinition(..)
  , InputSchemaDefinition(..)
  , InputSchemaDefinitionProperty(..)
  )


data McpOptions = McpOptions
  { backend        :: Maybe Backend
  , doNotUseSolver :: Bool
  , analysisDb     :: Maybe FilePath
  , typeHintsFile  :: Maybe FilePath
  , inputFile      :: FilePath
  }
  deriving (Eq, Ord, Read, Show, Generic)

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

parseTypeHintsFile :: Parser FilePath
parseTypeHintsFile = strOption $
  long "typeHints"
  <> short 't'
  <> metavar "TYPEHINT_FILE"
  <> help "file containing functions that we should get type hints for"

parseInputFile :: Parser FilePath
parseInputFile = argument str $
  metavar "INPUT_FILE"
  <> help "input binary file (e.g. .gzf)"

optionsParser :: Parser McpOptions
optionsParser = McpOptions
  <$> optional parseBackend
  <*> (parseDoNotUseSolver <|> pure False)
  <*> optional parseAnalysisDb
  <*> optional parseTypeHintsFile
  <*> parseInputFile

main :: IO ()
main = do
  opts <- execParser optsParser
  let fp = opts ^. #inputFile
  exists <- doesFileExist fp
  unless exists $ do
    SIO.hPutStrLn stderr $ "Error: file not found: " <> fp
    exitFailure
  SIO.hPutStrLn stderr $ "Loading " <> fp <> "..."
  withBackend (opts ^. #backend) fp $ \imp -> do
    typeHintsWhitelist <- maybe (pure HashSet.empty) getFuncsFromFile (opts ^. #typeHintsFile)
    (store, _funcToTypeHintsMap) <- Store.initWithTypeHints typeHintsWhitelist HashSet.empty (opts ^. #analysisDb) imp
    base <- getBase imp
    shellState <- initShellState store base (not $ opts ^. #doNotUseSolver)
    SIO.hPutStrLn stderr "Binary loaded. Starting MCP server..."
    runMcpServerStdio serverInfo (handlers shellState)
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
      , "Use 'list_functions' to discover functions in the loaded binary."
      , "Use 'sample_paths' to sample execution paths from a function."
      , "Use 'show_paths' to view the PIL statements on a sampled path."
      , "Use 'check_wmi' to check if a path matches a vulnerability pattern."
      , "Use 'list_wmis' to see all available vulnerability patterns."
      , "Typical workflow: list_functions -> sample_paths -> show_paths -> check_wmi"
      ]
  }


-- | MCP handlers: only tools, no prompts or resources
handlers :: ShellState -> McpServerHandlers IO
handlers st = McpServerHandlers
  { prompts   = Nothing
  , resources = Nothing
  , tools     = Just (pure toolDefinitions, handleToolCall st)
  }


-- | Convert a CommandResult to text for MCP response
renderResultText :: CommandResult -> Text
renderResultText = \case
  ResultText t -> t
  ResultOk t -> t
  ResultError t -> "Error: " <> t
  ResultFunctions funcs ->
    Text.unlines $ fmap (\(name, addr) -> show addr <> "  " <> name) funcs
  ResultPaths paths ->
    Text.unlines $ fmap (\(pid, summary) -> "[" <> show pid <> "] " <> summary) paths
  ResultSolver results ->
    Text.unlines $ fmap (\(pid, res) -> "[" <> show pid <> "] " <> res) results
  ResultWMIs results ->
    Text.unlines $ concatMap (\(pid, msgs) ->
      ("[" <> show pid <> "]") : msgs) results


-- | Dispatch an MCP tool call by building a shell command string
handleToolCall :: ShellState -> Text -> [(Text, Text)] -> IO (Either Error Content)
handleToolCall st "set_solver" args =
  case lookupArg "enabled" args of
    Nothing -> pure $ Left $ InvalidParams "Missing required parameter: enabled"
    Just val
      | val `elem` ["true", "1", "on"] -> do
          writeIORef (st ^. #useSolver) True
          pure $ Right $ ContentText "Solver enabled."
      | val `elem` ["false", "0", "off"] -> do
          writeIORef (st ^. #useSolver) False
          pure $ Right $ ContentText "Solver disabled."
      | otherwise -> pure $ Left $ InvalidParams $ "Invalid value for enabled: " <> val
handleToolCall st toolName args = do
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
            countPart = maybe "" (" " <>) count
            addrPart = maybe "" (\a -> " @ " <> a) addrs
        in Right $ "sample " <> func <> countPart <> addrPart

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

  "reduce_paths" ->
    case lookupArg "path_ids" args of
      Nothing -> Left "Missing required parameter: path_ids"
      Just pids -> Right $ "reduce " <> pids

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

  -- set_solver is handled directly in handleToolCall, not via command dispatch
  "set_solver" -> Left "handled_directly"

  _ -> Left $ "Unknown tool: " <> toolName


lookupArg :: Text -> [(Text, Text)] -> Maybe Text
lookupArg key = fmap snd . find (\(k, _) -> k == key)


-- | MCP tool definitions
toolDefinitions :: [ToolDefinition]
toolDefinitions =
  [ ToolDefinition
      { toolDefinitionName = "list_functions"
      , toolDefinitionDescription = "List functions in the binary. Optionally filter by substring."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("filter", InputSchemaDefinitionProperty "string" "Optional substring to filter function names")
              ]
          , required = []
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "sample_paths"
      , toolDefinitionDescription = "Sample execution paths from a function. Paths are cached and referenced by path ID."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("function", InputSchemaDefinitionProperty "string" "Function name or hex address (e.g. 'main' or '0x401000')")
              , ("count", InputSchemaDefinitionProperty "string" "Number of paths to sample (optional)")
              , ("addresses", InputSchemaDefinitionProperty "string" "Space-separated hex addresses that paths must pass through (optional)")
              ]
          , required = ["function"]
          }
      , toolDefinitionTitle = Nothing
      }
  , ToolDefinition
      { toolDefinitionName = "show_paths"
      , toolDefinitionDescription = "Show PIL (Platform Independent Language) statements for cached paths."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("path_ids", InputSchemaDefinitionProperty "string" "Path IDs to show (e.g. '0 1 2', '0..5', '[0,1,2]')")
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
      { toolDefinitionName = "reduce_paths"
      , toolDefinitionDescription = "Reduce paths via copy/constant propagation. Creates new cached path IDs with simplified statements."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("path_ids", InputSchemaDefinitionProperty "string" "Path IDs to reduce (e.g. '0 1 2', '0..5')")
              ]
          , required = ["path_ids"]
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
      , toolDefinitionDescription = "Check paths against a WMI vulnerability primitive. Returns variable mappings and locations if a match is found."
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties =
              [ ("wmi_name", InputSchemaDefinitionProperty "string" "Name of the WMI primitive (use list_wmis to see available)")
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
  ]
