{-# LANGUAGE TemplateHaskell #-}

module Blaze.Types.Cfg where

import Blaze.Prelude
import Blaze.Types.CallGraph (Function)
import qualified Blaze.Graph as Graph
import Blaze.Graph (Graph)
import Blaze.Types.Graph.Alga (AlgaGraph)
import Control.Arrow ((&&&))
import Data.BinaryAnalysis (Address)
import Prelude (error)

-- TODO: Consider adding more depending on what is being represented.
data BranchType
  = TrueBranch
  | FalseBranch
  | UnconditionalBranch
  deriving (Eq, Ord, Show, Generic)

instance Hashable BranchType

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
        _branchType :: BranchType
      }
  deriving (Eq, Ord, Show, Generic)

instance Hashable CfEdge

$(makeFieldsNoPrefix ''CfEdge)

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
    (_branchType &&& (_src &&& _dst)) <$> es

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

-- TODO: Separate graph construction from graph use
instance Graph BranchType CfNode (Cfg a) where
  empty = error "The empty function is unsupported for CFGs."
  fromNode _ = error "Use buildCfg to construct a CFG."
  fromEdges _ = error "Use buildCfg to construct a CFG."
  succs node = Graph.succs node . _graph
  preds node = Graph.preds node . _graph
  nodes = Graph.nodes . _graph 
  edges = Graph.edges . _graph
  getEdgeLabel edge = Graph.getEdgeLabel edge . _graph
  setEdgeLabel label edge cfg = cfg { _graph = Graph.setEdgeLabel label edge . _graph $ cfg }
  removeEdge edge cfg = cfg { _graph = Graph.removeEdge edge . _graph $ cfg }
  removeNode node cfg = cfg { _graph = Graph.removeNode node . _graph $ cfg }
  addNodes nodes cfg = cfg { _graph = Graph.addNodes nodes . _graph $ cfg }
  addEdge lblEdge cfg = cfg { _graph = Graph.addEdge lblEdge . _graph $ cfg }
  hasNode node = Graph.hasNode node . _graph