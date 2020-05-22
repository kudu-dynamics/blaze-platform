{-# LANGUAGE TemplateHaskell #-}

module Blaze.Types.Cfg where

import Blaze.Prelude
import Blaze.Types.CallGraph (Function)
import Blaze.Types.Graph.Alga (AlgaGraph)
import Data.BinaryAnalysis (Address)

-- TODO: Consider adding more depending on what is being represented.
data BranchType = TrueBranch 
                | FalseBranch
                | UnconditionalBranch

type ControlFlowGraph = AlgaGraph (Maybe BranchType) CfNode

data CfNode
  = BasicBlock
      { _basicBlockFunction :: Function,
        _basicBlockStart :: Address
      }
  | Call {  _callFunction :: Function,
            _callTarget :: Address }
  deriving (Eq, Ord, Show, Generic)

instance Hashable CfNode

$(makeFields ''CfNode)

data CfEdge
  = CfEdge
      { _src :: CfNode,
        _dst :: CfNode
      }
  deriving (Eq, Ord, Show)

data Cfg a
  = Cfg
      { _graph :: ControlFlowGraph,
        _mapping :: Maybe a
      }

buildCfg :: [CfNode] -> [CfEdge] -> Maybe a -> Cfg a
buildCfg ns es mapping = 
  Cfg { _graph = graph,
        _mapping = mapping }
    where
      graph :: ControlFlowGraph
      graph = undefined