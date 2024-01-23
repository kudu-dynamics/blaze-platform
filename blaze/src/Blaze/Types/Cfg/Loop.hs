module Blaze.Types.Cfg.Loop where

import Blaze.Prelude
import Blaze.Types.Cfg (CfEdge, Cfg)

newtype BackEdge n = BackEdge {edge :: CfEdge n}
  deriving (Eq, Show, Generic)
  deriving anyclass Hashable

-- TODO: Consider using a type that can only be a basic block node?
newtype LoopHeader n = LoopHeader {node :: n}
  deriving (Eq, Show, Generic)

newtype LoopBody n = LoopBody {nodes :: HashSet n}
  deriving (Eq, Show, Generic)

newtype LoopTail n = LoopTail {node :: n}
  deriving (Eq, Show, Generic)

newtype LoopNodes n = LoopNodes {nodes :: HashSet n}
  deriving (Eq, Show, Generic)

newtype LoopCfg n = LoopCfg {cfg :: Cfg n}
  deriving (Eq, Show, Generic)

data NatLoop a b = NatLoop
  { header :: LoopHeader a
  , body :: LoopBody a
  , tail :: LoopTail a
  , cfg :: LoopCfg a
  , backEdge :: BackEdge a
  }
  deriving (Eq, Show, Generic)

