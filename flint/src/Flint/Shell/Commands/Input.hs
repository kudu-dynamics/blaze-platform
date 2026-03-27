module Flint.Shell.Commands.Input
  ( inputGenesisCommand
  ) where

import Flint.Prelude

import Flint.Shell.Types
import Flint.Shell.Command (ShellCommand(..))
import Flint.Shell.Commands.Paths (expandPathToDepth, parseDepthArg)
import Flint.Types.Analysis (Parameter(..))
import qualified Flint.Cfg.Store as Store
import Flint.Types.Cfg.Store (CfgStore)
import Flint.Cfg.Path (samplesFromQuery, timingLog)
import Flint.Types.Analysis.Path.Matcher.PathPrep (mkPathPrep)
import Flint.Analysis.Path.Matcher (asStmts)
import Flint.Types.Query (QueryTargetOpts(..), Query(..))
import Flint.Util (timeIt)

import Blaze.Types.CallGraph (CallSite)
import qualified Blaze.Types.Function as Func
import qualified Blaze.Types.Cfg.Path as Path

import qualified Data.HashSet as HashSet
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import Numeric (showHex)

-- | Known libc/system input source functions with their controllable parameter.
knownInputSources :: [(Text, Parameter, Text)]
knownInputSources =
  [ ("recv",     Parameter 1, "network recv buffer")
  , ("recvfrom", Parameter 1, "network recvfrom buffer")
  , ("recvmsg",  Parameter 1, "network recvmsg buffer")
  , ("read",     Parameter 1, "file/socket read buffer")
  , ("fread",    Parameter 0, "stdio fread buffer")
  , ("fgets",    Parameter 0, "stdio fgets buffer")
  , ("gets",     Parameter 0, "stdin gets buffer")
  , ("scanf",    Parameter 1, "scanf output parameter")
  , ("fscanf",   Parameter 2, "fscanf output parameter")
  , ("sscanf",   Parameter 2, "sscanf output parameter")
  , ("getenv",   ReturnParameter, "environment variable value")
  ]

inputGenesisCommand :: ShellCommand
inputGenesisCommand = ShellCommand
  { cmdName = "input-genesis"
  , cmdAliases = ["ig"]
  , cmdHelp = "Auto-detect input sources and sample paths from their callers"
  , cmdUsage = "input-genesis [count] [--depth N]"
  , cmdAction = inputGenesisAction
  }

showAddr :: Address -> Text
showAddr addr = "0x" <> Text.pack (showHex (addrToInt addr) "")

showParamSpec :: Parameter -> Text
showParamSpec ReturnParameter = "ret"
showParamSpec (Parameter n) = "arg:" <> show n
showParamSpec (Other t) = "other:" <> t

-- | Names of entry point / runtime functions we should never sample from.
runtimeFunctions :: HashSet Text
runtimeFunctions = HashSet.fromList
  ["_start", "__libc_start_main", "__libc_csu_init", "__libc_csu_fini"
  , "_init", "_fini", "__do_global_dtors_aux", "frame_dummy"
  , "register_tm_clones", "deregister_tm_clones"
  , "main"
  ]

-- | Walk up the call chain from a set of functions, filtering out runtime
-- functions. Returns call sites at the highest reachable level only — if
-- callers have their own callers, we skip the current level and recurse.
-- This avoids sampling from thin wrappers like net_recv when event_loop
-- is available higher up the chain.
walkUpCallers :: CfgStore -> [Func.Func] -> Int -> HashSet Func.Function -> IO [CallSite]
walkUpCallers _store _funcs 0 _visited = return []
walkUpCallers _store [] _ _visited = return []
walkUpCallers store funcs maxLevels visited = do
  allCallSites <- fmap concat . forM funcs $
    Store.getCallSitesToFunc store

  let newCallSites = filter (\site ->
        let caller = site ^. #caller
        in not (HashSet.member caller visited)
           && not (HashSet.member (caller ^. #name) runtimeFunctions)
        ) allCallSites

  case newCallSites of
    [] -> return []
    _ -> do
      let callers = fmap (^. #caller) newCallSites
          uniqueCallers = HashSet.toList $ HashSet.fromList callers
          visited' = foldr HashSet.insert visited uniqueCallers

      -- Try to walk up further from the current callers
      higherResults <- walkUpCallers store
        (fmap Func.Internal uniqueCallers) (maxLevels - 1) visited'

      -- Only return this level if we couldn't go any higher
      if null higherResults
        then return newCallSites
        else return higherResults

-- | input-genesis [count] [--depth N]
-- Auto-detects input sources from the binary's extern imports, walks up the
-- call chain to find the highest-level callers, and samples paths through them.
inputGenesisAction :: ShellState -> [Text] -> IO CommandResult
inputGenesisAction st args = do
  let (depth, rest) = parseDepthArg args
      mCount = case rest of
        (n : _) -> readMaybe (Text.unpack n) :: Maybe Int
        _ -> Nothing
      count = fromMaybe 5 mCount
      store = st ^. #cfgStore

  -- Auto-detect input sources from extern imports
  externs <- Store.getExternalFuncs store
  let externNames = fmap (\e -> (Text.toLower (e ^. #name), e ^. #name)) externs
      detected = do
        (knownName, param, desc) <- knownInputSources
        (externLower, externOriginal) <- externNames
        guard $ externLower == Text.toLower knownName
        return (externOriginal, param, desc)

  case detected of
    [] -> return $ ResultOk "No known input source functions detected in this binary."
    _ -> do
      let detectLines = fmap (\(func, param, desc) ->
            "  " <> func <> " " <> showParamSpec param <> "  -- " <> desc) detected
          detectHeader = "Auto-detected " <> show (length detected) <> " input source(s):"

      -- For each detected source, walk up the call chain and sample
      internals <- Store.getInternalFuncs store
      allResults <- fmap concat . forM detected $ \(funcName, _param, _desc) -> do
        let matchingExterns = Func.External <$>
              filter (\e -> Text.toLower (e ^. #name) == Text.toLower funcName) externs
            matchingInternals = Func.Internal <$>
              filter (\f -> Text.toLower (f ^. #name) == Text.toLower funcName) internals
            matchingFuncs = matchingExterns <> matchingInternals

        -- Walk up to 5 levels to find the highest-level callers
        callSites <- walkUpCallers store matchingFuncs 5 HashSet.empty

        fmap concat . forM callSites $ \callSite -> do
          let caller = callSite ^. #caller
              callAddr = callSite ^. #address
              targets = NE.singleton (caller, callAddr)
              q = QueryTarget $ QueryTargetOpts
                { mustReachSome = targets
                , callExpandDepthLimit = 0
                , numSamples = fromIntegral count
                , unrollLoops = False
                }

          (paths, samplingTime) <- timeIt $
            catch
              (samplesFromQuery store caller q)
              (\(e :: SomeException) -> do
                warn $ "Sampling error for " <> caller ^. #name <> ": " <> show e
                return [])

          timingLog $ "[timing] input-genesis (" <> caller ^. #name <> " @ "
            <> show callAddr <> "): "
            <> show (length paths) <> " paths in " <> show samplingTime

          -- Auto-expand calls if depth > 0
          expandedPaths <- if depth <= 0
            then return paths
            else fmap concat . forM paths $ \path ->
              expandPathToDepth store path depth

          forM expandedPaths $ \path -> do
            let stmts = Path.toStmts path
                prep = mkPathPrep [] stmts
                reducedStmts = asStmts $ prep ^. #stmts
            pid <- insertPath st CachedPath
              { pilPath = stmts
              , fullPath = path
              , sourceFunc = caller
              , pathPrep = Just prep
              }
            let summary = "path " <> show pid
                  <> " (" <> show (length reducedStmts) <> " stmts"
                  <> ", input: " <> funcName
                  <> ", caller: " <> caller ^. #name
                  <> " @ " <> showAddr callAddr <> ")"
            return (pid, summary)

      let headerText = Text.unlines (detectHeader : detectLines)
      case allResults of
        [] -> return $ ResultText $ headerText <> "\nNo paths sampled from detected input sources."
        _ -> return $ ResultTextAndPaths headerText allResults
