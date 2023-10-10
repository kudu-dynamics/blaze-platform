module Flint.Types.Cfg.Store ( module Flint.Types.Cfg.Store ) where

import Flint.Prelude

import Blaze.Types.Function (Function)

import Blaze.Types.CallGraph (CallGraph, CallSite)
import Blaze.Types.Cfg (PilCfg, PilNode, CallNode)
import Blaze.Types.Graph (DescendantsMap)
import Blaze.Types.Pil (Stmt)
import Flint.Types.CachedCalc (CachedCalc)


data CfgInfo = CfgInfo
  { cfg :: PilCfg
  , acyclicCfg :: PilCfg
  , descendantsMap :: DescendantsMap PilNode
  , calls :: [CallNode [Stmt]]
  } deriving (Eq, Ord, Show, Generic)

-- | Mapping of function name to its cfg.
-- TODO: make this into sqlite db
data CfgStore = CfgStore
  { cfgCache :: CachedCalc Function (Maybe CfgInfo)
  , ancestorsCache :: CachedCalc Function (HashSet Function)
  , funcs :: [Function] -- result of `getFunctions` from CallGraph importer
  -- a dirty trick to use the CC machinery to get CG on demand
  , callGraphCache :: CachedCalc () CallGraph
  -- CallGraph, but all the edges are reversed. Useful for getting ancestors
  , transposedCallGraphCache :: CachedCalc () CallGraph
  , callSitesCache :: CachedCalc Function [CallSite]
  } deriving (Generic)
