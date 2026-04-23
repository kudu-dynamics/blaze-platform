-- | Shell commands for interprocedural data flow analysis.
module Flint.Shell.Commands.Dataflow
  ( dataflowSummaryCommand
  , dataflowScanCommand
  , dataflowQueryCommand
  , dataflowQueryAction
  ) where

import Flint.Prelude

import Flint.Shell.Types
import Flint.Shell.Command (ShellCommand(..))
import Flint.Shell.Commands.Paths (findFunction, findFunc)
import Flint.Types.Analysis.Dataflow
import Flint.Analysis.Dataflow.Summary
  ( computeSummary
  , getComposedDataflowStore
  )
import Flint.Analysis.Dataflow.Query (queryFlow)
import Flint.Analysis.Dataflow.DangerScan
import qualified Flint.Cfg.Store as Store
import Flint.Util (timeIt)

import Blaze.Types.Function (FuncRef(..), toFunctionRef, funcRefName)
import Data.Time.Clock (NominalDiffTime)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text


-- | Show data flow summary for a single function.
dataflowSummaryCommand :: ShellCommand
dataflowSummaryCommand = ShellCommand
  { cmdName = "dataflow-summary"
  , cmdAliases = ["dfs"]
  , cmdHelp = "Show data flow summary for a function (which params flow where)."
  , cmdUsage = "dataflow-summary <function>"
  , cmdAction = dataflowSummaryAction
  }

-- | Scan all functions and report dangerous data flow patterns.
dataflowScanCommand :: ShellCommand
dataflowScanCommand = ShellCommand
  { cmdName = "dataflow-scan"
  , cmdAliases = ["dfscan"]
  , cmdHelp = "Scan for dangerous data flows (tainted input -> unsafe sinks). "
           <> "--verbose for all summaries. --include-libc to show libc "
           <> "self-implementations (printf->vprintf etc.) normally filtered."
  , cmdUsage = "dataflow-scan [--verbose] [--include-libc]"
  , cmdAction = dataflowScanAction
  }

-- | Query interprocedural data flow between two endpoints.
dataflowQueryCommand :: ShellCommand
dataflowQueryCommand = ShellCommand
  { cmdName = "dataflow-query"
  , cmdAliases = ["dfq"]
  , cmdHelp = "Query if data flows from one function param to another across calls."
  , cmdUsage = "dataflow-query <src_func> <param_idx> <sink_func> <param_idx|ret>"
  , cmdAction = dataflowQueryAction
  }


-- | Fetch the composed 'DataflowStore' for the current binary, going
-- through the 'CfgStore'-level cache. Returns @(store, buildTime)@ —
-- @buildTime@ is the elapsed time for this call, which will be near
-- zero if the composed cache was already warm. The cache lives on
-- 'CfgStore' so any consumer (flint CLI, shell, MCP) shares it.
getOrBuildDataflowStore
  :: ShellState -> DataflowConfig -> IO (DataflowStore, NominalDiffTime)
getOrBuildDataflowStore st config =
  timeIt $ getComposedDataflowStore (st ^. #cfgStore) config

-- | Action for dataflow-summary: compute and display a single function's summary.
dataflowSummaryAction :: ShellState -> [Text] -> IO CommandResult
dataflowSummaryAction st args = case args of
  [] -> return $ ResultError "Usage: dataflow-summary <function>"
  (funcName : _) -> do
    mFunc <- findFunction st funcName
    case mFunc of
      Nothing -> return $ ResultError $ "Function not found: " <> funcName
      Just func -> do
        let store = st ^. #cfgStore
        mCfgInfo <- Store.getFuncCfgInfo store func
        case mCfgInfo of
          Nothing -> return $ ResultError $ "No CFG available for: " <> funcName
          Just cfgInfo -> do
            let ref = InternalRef (toFunctionRef func)
                config = defaultDataflowConfig
                summary = computeSummary config ref func cfgInfo
            return $ ResultText $ formatSummary summary

-- | Action for dataflow-scan: scan for dangerous data flow patterns.
dataflowScanAction :: ShellState -> [Text] -> IO CommandResult
dataflowScanAction st args = do
  let verbose = "--verbose" `elem` args || "-v" `elem` args
      includeLibc = "--include-libc" `elem` args
      scanOpts = defaultDangerScanOptions
        { excludeLibcPlumbing = not includeLibc }
      config = defaultDataflowConfig
  (dfStore, elapsed) <- getOrBuildDataflowStore st config
  let summaries = dfStore ^. #summaries
      interesting = HashMap.filter
                      (\s -> not (null (s ^. #flowEdges)) && s ^. #computed)
                      summaries
      total = HashMap.size summaries
      computed = HashMap.size $ HashMap.filter (^. #computed) summaries
      withFlows = HashMap.size interesting

      -- Run danger scan
      findings = dangerScanStoreWith scanOpts dfStore
      criticals = filter (\f -> f ^. #severity == Critical) findings
      highs = filter (\f -> f ^. #severity == High) findings
      mediums = filter (\f -> f ^. #severity == Medium) findings

      suppressedNote = if includeLibc
        then ""
        else "  (" <> show (length (dangerScanStoreWith
                                     (scanOpts { excludeLibcPlumbing = False })
                                     dfStore) - length findings)
             <> " libc-plumbing finding(s) suppressed; pass --include-libc to see)"

      header = Text.unlines
        [ "Data flow scan complete (" <> show elapsed <> ")"
        , "  Total functions: " <> show total
        , "  Analyzed (with CFG): " <> show computed
        , "  With flow edges: " <> show withFlows
        , ""
        , "Danger scan: " <> show (length findings) <> " finding(s)"
        , "  Critical: " <> show (length criticals)
        , "  High:     " <> show (length highs)
        , "  Medium:   " <> show (length mediums)
        , suppressedNote
        ]

      findingLines = if null findings
        then ["  (no dangerous patterns detected)"]
        else formatFinding
           <$> sortOn (\f -> (Down (f ^. #severity), funcRefName (f ^. #callerRef)))
                 findings

      verboseSection = if verbose
        then "\n\nAll functions with flow edges:\n"
           <> Text.unlines
                (fmap formatSummaryBrief
                 $ sortOn (\s -> funcRefName (s ^. #funcRef))
                 $ HashMap.elems interesting)
        else ""

  return $ ResultText $ header <> "\n" <> Text.unlines findingLines <> verboseSection

-- | Action for dataflow-query: check interprocedural flow.
dataflowQueryAction :: ShellState -> [Text] -> IO CommandResult
dataflowQueryAction st args = case args of
  [srcFuncName, srcParamStr, sinkFuncName, sinkParamStr] -> do
    -- Resolve both endpoints against internal AND extern functions.
    -- The common use case is a sink like 'free' or 'strcpy', which in
    -- a dynamically-linked binary is an extern — not an internal.
    mSrc  <- findFunc st srcFuncName
    mSink <- findFunc st sinkFuncName
    case (mSrc, mSink) of
      (Nothing, _) -> return $ ResultError $ "Source function not found: " <> srcFuncName
      (_, Nothing) -> return $ ResultError $ "Sink function not found: " <> sinkFuncName
      (Just (_, srcRef), Just (_, sinkRef)) -> do
        let config = defaultDataflowConfig

        case (parseEndpoint srcParamStr, parseEndpoint sinkParamStr) of
          (Left err, _) -> return $ ResultError $
            "Invalid source param '" <> srcParamStr <> "': " <> err
          (_, Left err) -> return $ ResultError $
            "Invalid sink param '" <> sinkParamStr <> "': " <> err
          (Right srcEp, Right sinkEp) -> do
            (dfStore, buildTime) <- getOrBuildDataflowStore st config
            cg <- Store.getCallGraph (st ^. #cfgStore)
            let query = FlowQuery (srcRef, srcEp) (sinkRef, sinkEp) Nothing
            (result, queryTime) <- timeIt $ return $! queryFlow dfStore cg query
            let status = if result ^. #reachable then "REACHABLE" else "NOT REACHABLE"
                pathCount = length (result ^. #paths)
                output = Text.unlines
                  [ "Query: " <> srcFuncName <> " " <> formatEndpoint srcEp
                      <> " -> " <> sinkFuncName <> " " <> formatEndpoint sinkEp
                  , "Result: " <> status
                  , "Paths found: " <> show pathCount
                  , "Build time: " <> show buildTime
                  , "Query time: " <> show queryTime
                  ]
            return $ ResultText output
  _ -> return $ ResultError
    "Usage: dataflow-query <src_func> <param_idx> <sink_func> <param_idx|ret>"

-- | Parse an endpoint argument: an integer index or \"ret\"/\"return\".
-- Returns an error rather than silently falling back.
parseEndpoint :: Text -> Either Text FlowEndpoint
parseEndpoint = \case
  "ret"    -> Right ReturnEndpoint
  "return" -> Right ReturnEndpoint
  s -> case readMaybe (Text.unpack s) of
    Just idx | idx >= 0  -> Right $ ParamEndpoint idx
    Just _               -> Left "negative parameter index"
    Nothing              -> Left "expected a non-negative integer, \"ret\", or \"return\""


-- Formatting helpers

formatEndpoint :: FlowEndpoint -> Text
formatEndpoint (ParamEndpoint i) = "param[" <> show i <> "]"
formatEndpoint ReturnEndpoint = "return"
formatEndpoint (GlobalEndpoint addr) = "global@" <> show addr

formatFlowKind :: FlowKind -> Text
formatFlowKind DirectFlow = "direct"
formatFlowKind DerefReadFlow = "deref-read"
formatFlowKind DerefWriteFlow = "deref-write"
formatFlowKind CallArgFlow = "call-arg"
formatFlowKind CallReturnFlow = "call-return"
formatFlowKind PhiFlow = "phi"

formatFlowEdge :: FlowEdge -> Text
formatFlowEdge e =
  "  " <> formatEndpoint (e ^. #from)
  <> " -> " <> formatEndpoint (e ^. #to)
  <> " [" <> formatFlowKind (e ^. #kind) <> "]"

formatSummary :: FuncSummary -> Text
formatSummary s =
  let name = funcRefName (s ^. #funcRef)
      nParams = HashMap.size (s ^. #paramVars)
      nRetVars = HashSet.size (s ^. #retVars)
      edges = s ^. #flowEdges
      paramLines = HashMap.foldlWithKey' (\acc idx var ->
        acc <> ["  param[" <> show idx <> "]: " <> var ^. #symbol]
        ) [] (s ^. #paramVars)
  in Text.unlines $
    [ "Function: " <> name
    , "Parameters: " <> show nParams
    , "Return vars: " <> show nRetVars
    , "Flow edges (" <> show (length edges) <> "):"
    ] <> fmap formatFlowEdge edges
      <> (if null paramLines then [] else "" : "Parameter mapping:" : paramLines)

formatSummaryBrief :: FuncSummary -> Text
formatSummaryBrief s =
  let name = funcRefName (s ^. #funcRef)
      nEdges = length (s ^. #flowEdges)
      nParams = HashMap.size (s ^. #paramVars)
  in "  " <> name <> " (" <> show nParams <> " params, "
     <> show nEdges <> " flow edges)"

formatFinding :: DangerFinding -> Text
formatFinding f =
  let sevStr = case f ^. #severity of
        Critical -> "[CRITICAL]"
        High     -> "[High]    "
        Medium   -> "[Medium]  "
        Info     -> "[Info]    "
      callerName = funcRefName (f ^. #callerRef)
      calleeName = funcRefName (f ^. #calleeRef)
      dangerStr = case f ^. #kind of
        UnboundedCopy    -> "unbounded-copy"
        FormatString     -> "format-string"
        HeapCorruption   -> "heap-corruption"
        CommandInjection -> "command-injection"
        BufferOverflow   -> "buffer-overflow"
        TaintedArithmetic -> "tainted-arithmetic"
  in "  " <> sevStr <> " " <> dangerStr <> "\n"
  <> "    " <> callerName <> " param[" <> show (f ^. #callerParam) <> "]"
  <> " -> " <> calleeName <> " param[" <> show (f ^. #sinkParam) <> "]"
  <> "\n    " <> f ^. #description
