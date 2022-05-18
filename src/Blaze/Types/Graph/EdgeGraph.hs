module Blaze.Types.Graph.EdgeGraph where

import Blaze.Prelude
import qualified Blaze.Types.Graph as G
import Blaze.Types.Graph (LEdge (LEdge), Edge (Edge), Graph, Identifiable, NodeId (NodeId))
import Blaze.Types.Cfg (BranchType, CfNode)
import qualified Data.HashSet as HSet
import Blaze.Types.Graph.Alga (AlgaGraph)

type EdgeGraph a = AlgaGraph () (EdgeGraphNode BranchType a) (EdgeGraphNode BranchType a)

data EdgeGraphNode l n
  = EdgeNode (LEdge l n)
  | NodeNode n
  deriving (Eq, Ord, Generic, Hashable)

instance Hashable a => Identifiable (EdgeGraphNode BranchType (CfNode a)) Int where
  -- TODO: What should the ID be?
  getNodeId n = NodeId $ hash n

instance Functor (EdgeGraphNode e) where
  fmap f (NodeNode x) = NodeNode $ f x
  fmap f (EdgeNode (LEdge e (Edge a b))) = EdgeNode . LEdge e $ Edge (f a) (f b)

-- | Converts to graph where edges are nodes.
toEdgeGraph :: forall l n g g'.
  ( Graph l n g
  , Graph () (EdgeGraphNode l n) g'
  , Hashable n
  ) => g n -> g' (EdgeGraphNode l n)
toEdgeGraph g = G.addNodes nodelist $ G.fromEdges edges'
     where
       nodelist :: [EdgeGraphNode e n]
       nodelist = fmap NodeNode . HSet.toList . G.nodes $ g

       edges' :: [LEdge () (EdgeGraphNode l n)]
       edges' = concatMap f $ G.edges g
         where
           f (LEdge e (Edge a b)) =
             [ LEdge () $ Edge (NodeNode a) edge'
             , LEdge () $ Edge edge' (NodeNode b)
             ]
             where
               edge' = EdgeNode (LEdge e $ Edge a b)
