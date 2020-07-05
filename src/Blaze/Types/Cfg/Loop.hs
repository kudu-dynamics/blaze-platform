{-# LANGUAGE TemplateHaskell #-}

module Blaze.Types.Cfg.Loop where

import Blaze.Prelude
import Blaze.Types.Cfg (CfEdge, CfNode, Cfg)

newtype BackEdge = BackEdge {_edge :: CfEdge}
  deriving (Eq, Show, Generic, Hashable)

$(makeFieldsNoPrefix ''BackEdge)

-- TODO: Consider using a type that can only be a basic block node?
newtype LoopHeader = LoopHeader {_node :: CfNode}
  deriving (Eq, Show)

$(makeFieldsNoPrefix ''LoopHeader)

newtype LoopBody = LoopBody {_nodes :: HashSet CfNode}
  deriving (Eq, Show)

$(makeFieldsNoPrefix ''LoopBody)

newtype LoopTail = LoopTail {_node :: CfNode}
  deriving (Eq, Show)

$(makeFieldsNoPrefix ''LoopTail)

newtype LoopNodes = LoopNodes {_nodes :: HashSet CfNode}
  deriving (Eq, Show)

$(makeFieldsNoPrefix ''LoopNodes)

newtype LoopCfg a = LoopCfg {_cfg :: Cfg a}
  deriving (Eq, Show)

$(makeFieldsNoPrefix ''LoopCfg)

data NatLoop a
  = NatLoop
      { _header :: LoopHeader,
        _body :: LoopBody,
        _tail :: LoopTail,
        _cfg :: LoopCfg a,
        _backEdge :: BackEdge
      }
  deriving (Eq, Show)

$(makeFieldsNoPrefix ''NatLoop)
