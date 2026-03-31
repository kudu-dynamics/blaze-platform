module Flint.Types.Cfg.Store where

import Flint.Prelude

import Flint.Types.Analysis.Path.Matcher (TypedStmt)
import Flint.Types.Analysis.Path.Matcher.PathPrep (PathPrep)
import Flint.Types.Analysis.Path.Matcher.Primitives (CallableWMI, PrimSpec)

import Blaze.Types.Function (Func, Function)

import Blaze.Import.Xref (Xref)
import Blaze.Types.CallGraph (CallGraph, CallSite)
import Blaze.Types.Cfg (CallNode, PilCfg, PilNode)
import Blaze.Types.Graph (StrictDescendantsMap)
import Blaze.Types.Pil (Stmt)
import Flint.Types.CachedCalc (CachedCalc)
import Flint.Types.CachedMap (CachedMap)

data CfgInfo = CfgInfo
  { cfg :: !PilCfg
  , strictDescendantsMap :: !(StrictDescendantsMap PilNode) -- based off cfg
  , nodes :: !(HashSet PilNode)
  , calls :: [CallNode [Stmt]]
  }
  deriving (Eq, Ord, Show, Generic)

{- | Mapping of function name to its cfg.
TODO: make this into sqlite db
-}
data CfgStore = CfgStore
  { cfgCache :: CachedCalc Function (Maybe CfgInfo)
  , acyclicCfgCache :: CachedCalc Function (Maybe PilCfg)
  , acyclicDescendantsCache :: CachedCalc Function (Maybe (StrictDescendantsMap PilNode))
  , ancestorsCache :: CachedCalc Func (HashSet Func)
  , descendantsCache :: CachedCalc Func (HashSet Func)
  , funcs :: CachedCalc () [Func] -- result of `getFunctions` from CallGraph importer
  , internalFuncs :: CachedCalc () [Function]
  , callGraphCache :: CachedCalc () CallGraph
    -- CallGraph, but all the edges are reversed. Useful for getting ancestors
  , transposedCallGraphCache :: CachedCalc () CallGraph
    -- Call sites contained within a function (caller → [CallSite])
  , callSitesInFuncCache :: CachedCalc Function [CallSite]
    -- Call sites targeting a function (callee → [CallSite])
  , callSitesToFuncCache :: CachedCalc Func [CallSite]
  , pathSamples :: CachedMap Function [PathPrep TypedStmt]
  , callablePrims :: CachedMap (PrimSpec, Func) (HashSet CallableWMI)
    -- -- If this is too slow or uses too much memory, we could do just calls or landmarks
    -- , funcNodeDescendantsCache :: CachedCalc () (HashMap Function PilNode)
    -- , planMakerCtx :: CachedCalc () (PlanMakerCtx Function PilNode)
    -- -- | All nodes in all Cfgs. Getting this causes all CfgInfos to be calc'd

    -- will undergo signficant change.
    -- tuple indicates arbitrary (create, store, delete) capability
  , baseOffset :: Address
  , stringsMap :: HashMap Address Text
  , stringXrefs :: HashMap Address [Xref]
  }
  deriving (Generic)
