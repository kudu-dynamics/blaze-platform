module Flint.Types.Cfg.Store ( module Flint.Types.Cfg.Store ) where

import Flint.Prelude

import Blaze.Types.Function (Function)

import Blaze.Types.Cfg (PilCfg)
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TMVar (TMVar)
import Flint.Types.CachedCalc (CachedCalc)

-- | Mapping of function name to its cfg.
-- TODO: make this into sqlite db
newtype CfgStore = CfgStore
  { cache :: CachedCalc Function (Maybe PilCfg)
  } deriving (Generic)


