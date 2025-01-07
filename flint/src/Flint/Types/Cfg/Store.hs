module Flint.Types.Cfg.Store ( module Flint.Types.Cfg.Store ) where

import Flint.Prelude

import Blaze.Types.Function (Function)

import Blaze.Types.CallGraph (CallGraph, CallSite)
import Blaze.Types.Cfg (PilCfg, PilNode, CallNode)
import Blaze.Types.Graph (DescendantsMap, StrictDescendantsMap)
import Blaze.Types.Pil (Stmt)
import Flint.Types.CachedCalc (CachedCalc)


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
  , ancestorsCache :: CachedCalc Function (HashSet Function)
  , descendantsCache :: CachedCalc Function (HashSet Function)
  , funcs :: CachedCalc () [Function] -- result of `getFunctions` from CallGraph importer
  -- a dirty trick to use the CC machinery to get CG on demand
  , callGraphCache :: CachedCalc () CallGraph
  -- CallGraph, but all the edges are reversed. Useful for getting ancestors
  , transposedCallGraphCache :: CachedCalc () CallGraph
  -- Mapping of call sites that call Function
  , callSitesCache :: CachedCalc Function [CallSite]
  -- -- If this is too slow or uses too much memory, we could do just calls or landmarks
  -- , funcNodeDescendantsCache :: CachedCalc () (HashMap Function PilNode)
  -- , planMakerCtx :: CachedCalc () (PlanMakerCtx Function PilNode)
  -- -- | All nodes in all Cfgs. Getting this causes all CfgInfos to be calc'd
  } deriving (Generic)
