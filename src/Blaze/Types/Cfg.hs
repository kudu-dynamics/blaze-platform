module Blaze.Types.Cfg where

import qualified Blaze.Graph as Graph
import Blaze.Graph (Graph)
import Blaze.Prelude hiding (pred)
import Blaze.Types.CallGraph (Function)
import Blaze.Types.Graph.Alga (AlgaGraph)
import Control.Arrow ((&&&))

-- TODO: Consider adding more depending on what is being represented.
data BranchType
  = TrueBranch
  | FalseBranch
  | UnconditionalBranch
  deriving (Eq, Ord, Show, Generic)

instance Hashable BranchType

data CfNode
  = BasicBlock
      { function :: Function
      , start :: Address
      }
  | Call
      { function :: Function
      , target :: Address
      }
  deriving (Eq, Ord, Show, Generic)

instance Hashable CfNode

data CfEdge = CfEdge
  { src :: CfNode
  , dst :: CfNode
  , branchType :: BranchType
  }
  deriving (Eq, Ord, Show, Generic)

instance Hashable CfEdge

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
mkControlFlowGraph root' ns es =
  Graph.addNodes (root' : ns) . Graph.fromEdges $
    (view #branchType &&& (view #src &&& view #dst)) <$> es

data Cfg a = Cfg
  { graph :: ControlFlowGraph
  , root :: CfNode
  , mapping :: Maybe a
  }
  deriving (Eq, Show, Generic)

buildCfg :: CfNode -> [CfNode] -> [CfEdge] -> Maybe a -> Cfg a
buildCfg root' rest es mapping' =
  Cfg
    { graph = graph'
    , root = root'
    , mapping = mapping'
    }
  where
    graph' :: ControlFlowGraph
    graph' = mkControlFlowGraph root' rest es

-- TODO: Is there a deriving trick to have the compiler generate this?
-- TODO: Separate graph construction from graph use and/or graph algorithms
instance Graph BranchType CfNode (Cfg a) where
  empty = error "The empty function is unsupported for CFGs."
  fromNode _ = error "Use buildCfg to construct a CFG."
  fromEdges _ = error "Use buildCfg to construct a CFG."
  succs node = Graph.succs node . view #graph
  preds node = Graph.preds node . view #graph
  nodes = Graph.nodes . view #graph
  edges = Graph.edges . view #graph
  getEdgeLabel edge = Graph.getEdgeLabel edge . view #graph
  setEdgeLabel label edge cfg = cfg & #graph %~ Graph.setEdgeLabel label edge
  removeEdge edge = over #graph $ Graph.removeEdge edge
  removeNode node = over #graph $ Graph.removeNode node
  addNodes nodes = over #graph $ Graph.addNodes nodes
  addEdge lblEdge = over #graph $ Graph.addEdge lblEdge
  hasNode node = Graph.hasNode node . view #graph
  transpose = over #graph Graph.transpose
  bfs startNodes = Graph.bfs startNodes . view #graph

  -- TODO: Standard subgraph doesn't make sense for a rooted graph. How to remedy?
  subgraph pred = over #graph $ Graph.subgraph pred
