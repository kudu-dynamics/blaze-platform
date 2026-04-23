module Flint.Types.Cfg.Store where

import Flint.Prelude

import Flint.Types.Analysis.Path.Matcher (TypedStmt)
import Flint.Types.Analysis.Path.Matcher.PathPrep (PathPrep)
import Flint.Types.Analysis.Path.Matcher.Primitives (CallableWMI, PrimSpec)
import Flint.Types.Analysis.Dataflow (FuncSummary, DataflowStore)

import Blaze.Types.Function (ExternFunction, Func, Function, FuncRef, FunctionRef)

import Blaze.Import.Xref (Xref)
import Blaze.Types.CallGraph (CallGraph, CallSite)
import Blaze.Types.Cfg (CallNode, PilCfg, PilNode)
import Blaze.Types.Graph (Dominators, StrictDescendantsMap)
import Blaze.Types.Pil (Stmt)
import Flint.Types.CachedCalc (CachedCalc)
import Flint.Types.CachedMap (CachedMap)
import Blaze.Types.PersistentCalc (PersistentCalc)
import Blaze.Concurrent (WorkerPool)

data CfgInfo = CfgInfo
  { cfg :: !PilCfg
  , strictDescendantsMap :: !(StrictDescendantsMap PilNode) -- based off cfg
  , dominators :: !(Dominators PilNode) -- for loop back edge detection
  , nodes :: !(HashSet PilNode)
  , calls :: [CallNode [Stmt]]
  }
  deriving (Eq, Ord, Show, Generic)

{- | Mapping of function name to its cfg.
TODO: make this into sqlite db
-}
data CfgStore = CfgStore
  { cfgCache :: PersistentCalc Function (Maybe CfgInfo)
  , acyclicCfgCache :: CachedCalc Function (Maybe PilCfg)
  , acyclicDescendantsCache :: CachedCalc Function (Maybe (StrictDescendantsMap PilNode))
  , ancestorsCache :: CachedCalc FuncRef (HashSet FuncRef)
  , descendantsCache :: CachedCalc FuncRef (HashSet FuncRef)
  , funcs :: CachedCalc () [FuncRef]
  , internalFuncs :: CachedCalc () [FunctionRef]
  , funcCalc :: PersistentCalc FunctionRef Function
  , externFuncCalc :: CachedCalc FunctionRef ExternFunction -- lazy extern resolution cache
  , callGraphCache :: PersistentCalc () CallGraph
    -- CallGraph, but all the edges are reversed. Useful for getting ancestors
  , transposedCallGraphCache :: CachedCalc () CallGraph
    -- Call sites contained within a function (caller → [CallSite])
  , callSitesInFuncCache :: CachedCalc Function [CallSite]
    -- Call sites targeting a function (callee → [CallSite])
  , callSitesToFuncCache :: CachedCalc FuncRef [CallSite]
  , pathSamples :: CachedMap Function [PathPrep TypedStmt]
  , callablePrims :: CachedMap (PrimSpec, Func) (HashSet CallableWMI)
    -- | Per-function intra-procedural dataflow summaries. LMDB-backed;
    -- computed lazily on first access via 'setupDataflowCache' in
    -- Flint.Analysis.Dataflow.Summary.
  , dataflowCache :: PersistentCalc FuncRef FuncSummary
    -- | In-memory cache of the fully-composed 'DataflowStore'. Derived
    -- from 'dataflowCache' plus the call graph; cheap to recompose
    -- when the intras are warm. Lives here (not in ShellState) so
    -- every 'CfgStore' consumer — flint CLI, flint-shell, flint-mcp —
    -- shares one composed store per binary load.
  , composedDataflowCache :: CachedCalc () DataflowStore
    -- -- If this is too slow or uses too much memory, we could do just calls or landmarks
    -- , funcNodeDescendantsCache :: CachedCalc () (HashMap Function PilNode)
    -- , planMakerCtx :: CachedCalc () (PlanMakerCtx Function PilNode)
    -- -- | All nodes in all Cfgs. Getting this causes all CfgInfos to be calc'd

    -- will undergo signficant change.
    -- tuple indicates arbitrary (create, store, delete) capability
  , workerPool :: WorkerPool
  , baseOffset :: Address
  , stringsMap :: HashMap Address Text
  , stringXrefs :: HashMap Address [Xref]
  }
  deriving (Generic)
