-- | Interprocedural data flow queries.
--
-- Given a 'DataflowStore' (summaries already composed bottom-up by
-- 'Flint.Analysis.Dataflow.Summary.buildDataflowStore') and a call
-- graph, this module answers "can data flow from source to sink?"
--
-- The search combines two mechanisms:
--
--   * /Intra-procedural reachability/ within each visited function via
--     transitive closure over its composed 'flowEdges'. Because
--     composition has already inlined callee flows, this covers most
--     indirect cases.
--
--   * /Cross-function descent/ through 'callSites.argMapping'. Each
--     arg-mapping entry @(calleeParamIdx, callerEndpoint)@ is exactly
--     the bridge from a caller endpoint to the callee's positional
--     parameter, which the intra-procedural edges alone cannot express
--     (the caller summary has no notion of "callee X's param Y").
module Flint.Analysis.Dataflow.Query
  ( -- * Queries
    queryFlow
  , queryFlowAll
    -- * Utilities
  , funcSummaryFlowsFrom
  , summarizeFunction
  , intraReachable
  ) where

import Flint.Prelude

import Flint.Types.Analysis.Dataflow

import Blaze.Types.Function (FuncRef)
import Blaze.Types.CallGraph (CallGraph)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet


-- | Look up the summary for a function, returning an empty one if missing.
lookupSummary :: DataflowStore -> FuncRef -> FuncSummary
lookupSummary store ref =
  fromMaybe (emptySummary ref) $ HashMap.lookup ref (store ^. #summaries)

-- | Get all flow edges that originate from a given endpoint in a summary.
funcSummaryFlowsFrom :: FuncSummary -> FlowEndpoint -> [FlowEdge]
funcSummaryFlowsFrom summary ep =
  filter (\e -> e ^. #from == ep) (summary ^. #flowEdges)

-- | Summarize what a function does: which endpoints flow where.
-- Returns a map from source endpoint to destination endpoints.
summarizeFunction :: DataflowStore -> FuncRef -> HashMap FlowEndpoint [FlowEndpoint]
summarizeFunction store ref =
  let summary = lookupSummary store ref
  in foldl' addEdge HashMap.empty (summary ^. #flowEdges)
  where
    addEdge acc edge =
      HashMap.insertWith (<>) (edge ^. #from) [edge ^. #to] acc

-- | All endpoints reachable from @seed@ within a single function's
-- composed 'flowEdges' graph. Includes @seed@ itself.
intraReachable :: FuncSummary -> FlowEndpoint -> HashSet FlowEndpoint
intraReachable summary seed =
  let adj = buildAdjacency (summary ^. #flowEdges)
      initial = HashSet.singleton seed
  in go adj initial initial
  where
    go adj visited frontier
      | HashSet.null frontier = visited
      | otherwise =
          let next = HashSet.fromList
                [ to'
                | from' <- HashSet.toList frontier
                , to'   <- HashMap.lookupDefault [] from' adj
                , not (HashSet.member to' visited)
                ]
          in go adj (HashSet.union visited next) next

    buildAdjacency :: [FlowEdge] -> HashMap FlowEndpoint [FlowEndpoint]
    buildAdjacency =
      foldl' (\m e -> HashMap.insertWith (<>) (e ^. #from) [e ^. #to] m) HashMap.empty

-- | Answer a single flow query: can data reach from source to sink?
-- Performs a bounded DFS across function boundaries.
queryFlow :: DataflowStore -> CallGraph -> FlowQuery -> FlowResult
queryFlow store _cg query =
  let maxD = fromMaybe (store ^. #config . #maxCallDepth) (query ^. #maxDepth)
      (srcFunc, srcEp) = query ^. #source
      (sinkFunc, sinkEp) = query ^. #sink
      paths = searchFlow store maxD srcFunc srcEp sinkFunc sinkEp
  in FlowResult
      { query     = query
      , reachable = not (null paths)
      , paths     = paths
      }

-- | DFS for interprocedural flow paths from @(srcFunc, srcEp)@ to
-- @(sinkFunc, sinkEp)@.
--
-- State is @(FuncRef, FlowEndpoint)@ — the same function may be visited
-- multiple times via different entry endpoints. The visited set
-- prevents revisiting the same @(func, endpoint)@ pair within a single
-- query, which bounds exploration in cyclic call graphs.
searchFlow
  :: DataflowStore
  -> Int                -- ^ maximum remaining call depth
  -> FuncRef            -- ^ source function
  -> FlowEndpoint       -- ^ source endpoint
  -> FuncRef            -- ^ sink function
  -> FlowEndpoint       -- ^ sink endpoint
  -> [[FlowPath]]
searchFlow store maxDepth0 srcFunc0 srcEp0 sinkFunc sinkEp =
  go HashSet.empty maxDepth0 srcFunc0 srcEp0
  where
    go :: HashSet (FuncRef, FlowEndpoint) -> Int -> FuncRef -> FlowEndpoint -> [[FlowPath]]
    go visited depth curFunc curEp
      | depth <= 0 = []
      | HashSet.member (curFunc, curEp) visited = []
      | otherwise =
          let visited' = HashSet.insert (curFunc, curEp) visited
              summary = lookupSummary store curFunc
              reachable = intraReachable summary curEp

              -- Case 1: sink is inside curFunc and reachable from curEp
              -- via the composed intra-procedural edges.
              intraHits
                | curFunc == sinkFunc && HashSet.member sinkEp reachable =
                    [[FlowPath curFunc (FlowEdge curEp sinkEp DirectFlow)]]
                | otherwise = []

              -- Case 2: sink lies beyond a callsite. For each callsite
              -- and each arg-mapping entry, if the argSource is in
              -- this function's reachable set, treat it as a bridge
              -- into the callee's positional parameter.
              callHits = concatMap (descendCallSite visited' depth curFunc reachable)
                                   (summary ^. #callSites)

              -- Case 3: this function /wrote/ to some GlobalEndpoint
              -- during the current flow from curEp. Bridge to other
              -- functions that read that global. Read-to-read chains
              -- without an intermediate write are deliberately not
              -- bridged — that would manufacture a flow from two
              -- unrelated consumers of the same static memory.
              writtenGlobals = globalsWrittenFrom summary reachable
              globalHits = concatMap (bridgeGlobal visited' depth curFunc)
                                     (HashSet.toList writtenGlobals)

          in intraHits <> callHits <> globalHits

    descendCallSite
      :: HashSet (FuncRef, FlowEndpoint)
      -> Int
      -> FuncRef
      -> HashSet FlowEndpoint
      -> CallSiteDetail
      -> [[FlowPath]]
    descendCallSite visited depth curFunc reachable site =
      concat
        [ hitsFor argIdx argSrc
        | (argIdx, argSrc) <- site ^. #argMapping
        , HashSet.member argSrc reachable
        ]
      where
        calleeRef = site ^. #calleeRef
        hitsFor argIdx argSrc =
          let calleeEp = ParamEndpoint argIdx
              callerToArgEdge = FlowEdge argSrc calleeEp CallArgFlow
              callerStep = FlowPath curFunc callerToArgEdge
          in if calleeRef == sinkFunc && calleeEp == sinkEp
             then [[callerStep]]
             else (callerStep :)
                  <$> go visited (depth - 1) calleeRef calleeEp

    -- | Bridge to other functions that read the given global. Only
    -- called with endpoints known to be globals written to during the
    -- current function's flow (see 'globalsWrittenFrom').
    bridgeGlobal
      :: HashSet (FuncRef, FlowEndpoint)
      -> Int
      -> FuncRef
      -> FlowEndpoint
      -> [[FlowPath]]
    bridgeGlobal _ _ _ ReturnEndpoint    = []
    bridgeGlobal _ _ _ (ParamEndpoint _) = []
    bridgeGlobal visited depth curFunc ge@(GlobalEndpoint _) =
      [ FlowPath curFunc (FlowEdge ge ge DerefReadFlow) : subPath
      | (readerRef, _) <- globalReaders ge
      , readerRef /= curFunc
      , subPath <- go visited (depth - 1) readerRef ge
      ]

    -- | Every function whose composed summary contains either an edge
    -- with @from == ge@ (direct read of the global) or a callsite with
    -- @ge@ as an argMapping source (the global is passed to a callee).
    globalReaders :: FlowEndpoint -> [(FuncRef, FuncSummary)]
    globalReaders ge =
      [ (ref, s)
      | (ref, s) <- HashMap.toList (store ^. #summaries)
      , s ^. #computed
      , hasReaderEdge s ge || hasReaderArg s ge
      ]

    hasReaderEdge s ge =
      any (\e -> e ^. #from == ge) (s ^. #flowEdges)

    hasReaderArg s ge =
      any (\site -> any (\(_, src) -> src == ge) (site ^. #argMapping))
          (s ^. #callSites)

-- | Globals that the current in-function flow actually writes to.
-- A 'GlobalEndpoint g' is included only if the summary has some edge
-- whose destination is @GlobalEndpoint g@ and whose source is in the
-- given reachable set — i.e., data flowing from the query's current
-- endpoint reached a write to that global.
--
-- This prevents the query from modelling "read-to-read" propagation
-- across two consumers of the same static memory, which is not a real
-- data flow.
globalsWrittenFrom :: FuncSummary -> HashSet FlowEndpoint -> HashSet FlowEndpoint
globalsWrittenFrom summary reached = HashSet.fromList
  [ e ^. #to
  | e <- summary ^. #flowEdges
  , HashSet.member (e ^. #from) reached
  , isGlobal (e ^. #to)
  ]
  where
    isGlobal (GlobalEndpoint _) = True
    isGlobal _                  = False

-- | Run multiple flow queries against the same store.
queryFlowAll :: DataflowStore -> CallGraph -> [FlowQuery] -> [FlowResult]
queryFlowAll store cg = map (queryFlow store cg)
