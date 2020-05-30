{-# LANGUAGE TemplateHaskell #-}

module Blaze.Types.Cfg where

import Blaze.Prelude
import Blaze.Types.CallGraph (Function)
import qualified Blaze.Graph as Graph
import Blaze.Types.Graph.Alga (AlgaGraph)
import Control.Arrow ((&&&))
import Data.BinaryAnalysis (Address)

-- TODO: Consider adding more depending on what is being represented.
data BranchType
  = TrueBranch
  | FalseBranch
  | UnconditionalBranch
  deriving (Eq, Ord, Show)

data CfNode
  = BasicBlock
      { _basicBlockFunction :: Function,
        _basicBlockStart :: Address
      }
  | Call
      { _callFunction :: Function,
        _callTarget :: Address
      }
  deriving (Eq, Ord, Show, Generic)

instance Hashable CfNode

$(makeFields ''CfNode)

data CfEdge
  = CfEdge
      { _src :: CfNode,
        _dst :: CfNode,
        _type :: BranchType
      }
  deriving (Eq, Ord, Show)

$(makeFields ''CfEdge)

newtype Dominators = Dominators (HashMap CfNode (HashSet CfNode))

newtype PostDominators = PostDominators (HashMap CfNode (HashSet CfNode))

-- | A non-empty graph that consists of a strongly-connected component
-- with a single root node (a node with no incoming edges).
-- This is intended to be the graph representation of a CFG.
-- A user of this API probably wants to work with the 'Cfg' type that
-- includes additional information about the CFG.
type ControlFlowGraph = AlgaGraph BranchType CfNode

-- TODO: How to best "prove" this generates a proper ControlFlowGraph?
mkControlFlowGraph :: CfNode -> [CfNode] -> [CfEdge] -> ControlFlowGraph
mkControlFlowGraph root ns es =
  Graph.addNodes (root : ns) . Graph.fromEdges $
    (_type &&& (_src &&& _dst)) <$> es

data Cfg a
  = Cfg
      { _graph :: ControlFlowGraph,
        _root :: CfNode,
        _mapping :: Maybe a
      }

buildCfg :: CfNode -> [CfNode] -> [CfEdge] -> Maybe a -> Cfg a
buildCfg root rest es mapping =
  Cfg
    { _graph = graph,
      _root = root,
      _mapping = mapping
    }
  where
    graph :: ControlFlowGraph
    graph = mkControlFlowGraph root rest es
