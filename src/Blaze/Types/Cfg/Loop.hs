module Blaze.Types.Cfg.Loop where

import Blaze.Prelude
import Blaze.Types.Cfg (CfEdge, CfNode, Cfg)

newtype BackEdge a = BackEdge {edge :: CfEdge a}
  deriving (Eq, Show, Generic)
  deriving anyclass Hashable

-- TODO: Consider using a type that can only be a basic block node?
newtype LoopHeader a = LoopHeader {node :: CfNode a}
  deriving (Eq, Show, Generic)

newtype LoopBody a = LoopBody {nodes :: HashSet (CfNode a)}
  deriving (Eq, Show, Generic)

newtype LoopTail a = LoopTail {node :: CfNode a}
  deriving (Eq, Show, Generic)

newtype LoopNodes a = LoopNodes {nodes :: HashSet (CfNode a)}
  deriving (Eq, Show, Generic)

newtype LoopCfg a = LoopCfg {cfg :: Cfg a}
  deriving (Eq, Show, Generic)

data NatLoop a b = NatLoop
  { header :: LoopHeader a
  , body :: LoopBody a
  , tail :: LoopTail a
  , cfg :: LoopCfg a
  , backEdge :: BackEdge a
  }
  deriving (Eq, Show, Generic)

