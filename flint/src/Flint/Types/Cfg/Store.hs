module Flint.Types.Cfg.Store where

import Flint.Prelude

import Flint.Types.Analysis.Path.Matcher (TypedStmt)
import Flint.Types.Analysis.Path.Matcher.PathPrep (PathPrep)
import Flint.Types.Analysis.Path.Matcher.Primitives (CallableWMI, PrimSpec)

import Blaze.Types.Function (Function, Func)

import Blaze.Types.CallGraph (CallGraph, CallSite)
import Blaze.Types.Cfg (PilCfg, PilNode, CallNode)
import Blaze.Types.Graph (DescendantsMap, StrictDescendantsMap)
import Blaze.Types.Pil (Stmt)
import Flint.Types.CachedCalc (CachedCalc)
import Flint.Types.CachedMap (CachedMap)


data CfgInfo = CfgInfo
  { cfg :: PilCfg
  , acyclicCfg :: PilCfg
  , descendantsMap :: DescendantsMap PilNode -- based off of acyclicCfg
  , strictDescendantsMap :: StrictDescendantsMap PilNode -- based off cfg
  , nodes :: HashSet PilNode
  , calls :: [CallNode [Stmt]]
  } deriving (Eq, Ord, Show, Generic)

-- | Mapping of function name to its cfg.
-- TODO: make this into sqlite db
data CfgStore = CfgStore
  { cfgCache :: CachedCalc Function (Maybe CfgInfo)
  , ancestorsCache :: CachedCalc Func (HashSet Func)
  , descendantsCache :: CachedCalc Func (HashSet Func)
  , funcs :: CachedCalc () [Func] -- result of `getFunctions` from CallGraph importer
  , internalFuncs :: CachedCalc () [Function]
  , callGraphCache :: CachedCalc () CallGraph
  -- CallGraph, but all the edges are reversed. Useful for getting ancestors
  , transposedCallGraphCache :: CachedCalc () CallGraph
  -- Functions and the callsites they contain
  , callSitesCache :: CachedCalc Function [CallSite]

  , pathSamples :: CachedMap Function [PathPrep TypedStmt]
  , callablePrims :: CachedMap (PrimSpec, Func) (HashSet CallableWMI)
  -- -- If this is too slow or uses too much memory, we could do just calls or landmarks
  -- , funcNodeDescendantsCache :: CachedCalc () (HashMap Function PilNode)
  -- , planMakerCtx :: CachedCalc () (PlanMakerCtx Function PilNode)
  -- -- | All nodes in all Cfgs. Getting this causes all CfgInfos to be calc'd
  , baseOffset :: Address
  } deriving (Generic)
