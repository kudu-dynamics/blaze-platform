module Blaze.Types.Cfg.Loop where

import Blaze.Prelude
import Blaze.Types.Cfg (CfEdge, CfNode, Cfg)

newtype BackEdge = BackEdge {edge :: CfEdge}
  deriving (Eq, Show, Generic)

instance Hashable BackEdge

-- TODO: Consider using a type that can only be a basic block node?
newtype LoopHeader = LoopHeader {node :: CfNode}
  deriving (Eq, Show, Generic)

newtype LoopBody = LoopBody {nodes :: HashSet CfNode}
  deriving (Eq, Show, Generic)

newtype LoopTail = LoopTail {node :: CfNode}
  deriving (Eq, Show, Generic)

newtype LoopNodes = LoopNodes {nodes :: HashSet CfNode}
  deriving (Eq, Show, Generic)

newtype LoopCfg a = LoopCfg {cfg :: Cfg a}
  deriving (Eq, Show, Generic)

data NatLoop a = NatLoop
  { header :: LoopHeader
  , body :: LoopBody
  , tail :: LoopTail
  , cfg :: LoopCfg a
  , backEdge :: BackEdge
  }
  deriving (Eq, Show, Generic)
