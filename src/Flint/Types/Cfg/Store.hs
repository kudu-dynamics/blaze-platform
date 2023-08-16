module Flint.Types.Cfg.Store ( module Flint.Types.Cfg.Store ) where

import Flint.Prelude

import Blaze.Types.Function (Function)

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
newtype CfgStore = CfgStore
  { cache :: CachedCalc Function (Maybe CfgInfo)
  } deriving (Generic)
