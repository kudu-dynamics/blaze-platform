{-# LANGUAGE TemplateHaskell #-}

module Blaze.Types.Cfg.Loop where

import Blaze.Prelude
import Blaze.Types.Cfg (CfNode, CfEdge, Cfg)

newtype BackEdge = BackEdge { _edge :: CfEdge }
  deriving (Eq, Show, Generic, Hashable)

$(makeFieldsNoPrefix ''BackEdge)

-- TODO: Consider using a type that can only be a basic block node?
newtype LoopHeader = LoopHeader { _node :: CfNode}

$(makeFieldsNoPrefix ''LoopHeader)

newtype LoopBody a = LoopBody { _body :: Cfg a }

$(makeFieldsNoPrefix ''LoopBody)

data NatLoop a
  = NatLoop
      { _header :: LoopHeader,
        _body :: LoopBody a,
        _backEdge :: BackEdge
      }

$(makeFieldsNoPrefix ''NatLoop)