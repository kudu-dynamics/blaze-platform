module Blaze.Types.Cfg where

import Blaze.Graph (Graph)
import qualified Blaze.Graph as Graph
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

data CfNode a
  = BasicBlock
      { function :: Function
      , start :: Address
      , end :: Address
      , nodeData :: a
      }
  | Call
      { function :: Function
      , start :: Address
      , end :: Address
      , nodeData :: a
      }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

data CfEdge a = CfEdge
  { src :: CfNode a
  , dst :: CfNode a
  , branchType :: BranchType
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

mkEdge :: (BranchType, (CfNode a, CfNode a)) -> CfEdge a
mkEdge (bt, (s, d)) =
  CfEdge s d bt
  
data CodeReference a = CodeReference
  { function :: Function
  , startIndex :: a
  , endIndex :: a
  }
  deriving (Eq, Ord, Show, Generic)

type NodeRefMap a b = HashMap a b

type NodeRefMapEntry a b = (a, b)

newtype Dominators a = Dominators (HashMap (CfNode a) (HashSet (CfNode a)))
  deriving (Eq, Ord, Show, Generic)

newtype PostDominators a = PostDominators (HashMap (CfNode a) (HashSet (CfNode a)))
  deriving (Eq, Ord, Show, Generic)

{- | A non-empty graph that consists of a strongly-connected component
 with a single root node (a node with no incoming edges).
 This is intended to be the graph representation of a CFG.
 A user of this API probably wants to work with the 'Cfg' type that
 includes additional information about the CFG.
-}
type ControlFlowGraph a = AlgaGraph BranchType (CfNode a)

-- TODO: How to best "prove" this generates a proper ControlFlowGraph?
mkControlFlowGraph :: Ord a => CfNode a -> [CfNode a] -> [CfEdge a] -> ControlFlowGraph a
mkControlFlowGraph root' ns es =
  Graph.addNodes (root' : ns) . Graph.fromEdges $
    (view #branchType &&& (view #src &&& view #dst)) <$> es

-- TODO: Need to remove nodes from the mapping. Consider making mapping external,
--       an implementor can't know how to speculatively update this without
--       more type info and an appropriate interface
-- TODO: The mapping was originally intended to map imported information to
--       the source it was derived from. Consider removing mapping from Cfg and
--       instead providing it as an additional result after the importing process.
-- TODO: Expand CfNode so that it can support storing different information.
--       E.g., a CfNode could have a data :: a field which could be [Stmt] or
--       anything else. We may still need to support parameterized location
--       references for cases where Address won't work (e.g., MLIL SSA)
data Cfg a = Cfg
  { graph :: ControlFlowGraph a
  , root :: CfNode a
  }
  deriving (Eq, Show, Generic)

mkCfg :: forall a. Ord a => CfNode a -> [CfNode a] -> [CfEdge a] -> Cfg a
mkCfg root' rest es =
  Cfg
    { graph = mkControlFlowGraph root' rest es
    , root = root'
    }

-- TODO: Is there a deriving trick to have the compiler generate this?
-- TODO: Separate graph construction from graph use and/or graph algorithms
instance Ord a => Graph BranchType (CfNode a) (Cfg a) where
  empty = error "The empty function is unsupported for CFGs."
  fromNode _ = error "Use mkCfg to construct a CFG."
  fromEdges _ = error "Use mkCfg to construct a CFG."
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
