module Blaze.Types.Cfg.Loop where

import Blaze.Prelude
import Blaze.Types.Cfg (CfEdgeUnique, CfNode, Cfg)
import Blaze.Types.Graph.Unique (Unique)

newtype BackEdge a = BackEdge {edge :: CfEdgeUnique a}
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass Hashable

-- TODO: Consider using a type that can only be a basic block node?
newtype LoopHeader a = LoopHeader {node :: Unique (CfNode a)}
  deriving (Eq, Show, Generic)

newtype LoopBody a = LoopBody {nodes :: HashSet (Unique (CfNode a))}
  deriving (Eq, Show, Generic)

newtype LoopTail a = LoopTail {node :: Unique (CfNode a)}
  deriving (Eq, Show, Generic)

newtype LoopNodes a = LoopNodes {nodes :: HashSet (Unique (CfNode a))}
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
