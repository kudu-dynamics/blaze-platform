module Blaze.Types.Graph.Alga where

import Blaze.Prelude hiding (pred)

import qualified Algebra.Graph.AdjacencyMap as G
import qualified Algebra.Graph.AdjacencyMap.Algorithm as GA
import Control.Arrow ((&&&))
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet
import Blaze.Types.Graph hiding (edge, label, src, dst)
import qualified Algebra.Graph.Export.Dot as Dot


-- | A graph implementation that is build atop the Alga graph library.
-- The 'l' type specifies the edge label, the 'i' type specifies the 
-- node idenitifier, and the 'n' type specifies the node.
-- WARNING: The Functor and Traversable instances are not safe if
-- the NodeId is changed.
data AlgaGraph l i n = AlgaGraph
  -- TODO: We can move to AdjacencyIntMap if we assert NodeId is an Int
  { adjacencyMap :: G.AdjacencyMap (NodeId i)
  , edgeMap :: HashMap (Edge (NodeId i)) l
  , nodeMap :: HashMap (NodeId i) n
  } deriving (Generic, Show, Ord, Eq, Functor, Foldable, Traversable)

-- TODO: see if G.AdjacencyMap's Eq is good enough
-- I think that two graphs with identitcal nodes and edges will not be equal

-- | Call this if you want to map over the nodes and change the node ids.
safeMap
  :: ( Hashable a
     , Hashable b
     , Hashable i
     , Hashable j
     , Ord i
     , Ord j
     , Identifiable a i
     , Identifiable b j
     )
  => (a -> b)
  -> AlgaGraph l i a
  -> AlgaGraph l j b
safeMap f g
  = addNodes (fmap f . HSet.toList $ nodes g)
  . fromEdges
  . fmap (fmap f)
  . edges
  $ g

-- | Call this if you want to traverse over the nodes and change the node ids.
safeTraverse
  :: ( Hashable a
     , Hashable b
     , Hashable i
     , Hashable j
     , Ord i
     , Ord j
     , Identifiable a i
     , Identifiable b j
     , Applicative f
     )
  => (a -> f b)
  -> AlgaGraph l i a
  -> f (AlgaGraph l j b)
safeTraverse f g
  = addNodes
  <$> (traverse f . HSet.toList $ nodes g)
  <*> (fmap fromEdges . traverse (traverse f) . edges $ g)

instance (NFData l, NFData i, NFData n) => NFData (AlgaGraph l i n)

instance
  (Ord i, Hashable i, Hashable n, Identifiable n i) =>
  GraphConstruct l n (AlgaGraph l i)
  where
  empty = AlgaGraph G.empty HMap.empty HMap.empty
  fromNode node =
    AlgaGraph
      (G.vertex $ getNodeId node)
      HMap.empty
      (HMap.singleton (getNodeId node) node)
  fromEdges ledges =
    AlgaGraph
      { adjacencyMap = G.edges . fmap (toTupleEdge . fmap getNodeId . view #edge) $ ledges
      , edgeMap = HMap.fromList $ (\e -> (getNodeId <$> e ^. #edge, e ^. #label)) <$> ledges
      , nodeMap = HMap.fromList $ (\n -> (getNodeId n, n)) <$> HSet.toList (nodesFromEdges ledges)
      }
    
instance
  (Ord i, Hashable i, Hashable n, Identifiable n i) =>
  Graph l n (AlgaGraph l i)
  where
  succs n g =
    HSet.fromList . mapMaybe (getNode g) . Set.toList . G.postSet (getNodeId n) $ adjacencyMap g
  preds n g =
    HSet.fromList . mapMaybe (getNode g) . Set.toList . G.preSet (getNodeId n) $ adjacencyMap g
  nodes g = HSet.fromList . mapMaybe (getNode g) . G.vertexList $ adjacencyMap g
  edges g = mapMaybe (f . fromTupleEdge) . G.edgeList $ adjacencyMap g
   where
    f e = HMap.lookup e (edgeMap g) >>= traverse (getNode g) . flip LEdge e

  getEdgeLabel edge = HMap.lookup (fmap getNodeId edge) . edgeMap
  setEdgeLabel label edge g = g{edgeMap = HMap.insert (fmap getNodeId edge) label $ edgeMap g}

  removeEdge e@(Edge n1 n2) g =
    AlgaGraph
      { adjacencyMap = G.removeEdge (getNodeId n1) (getNodeId n2) $ adjacencyMap g
      , edgeMap = HMap.delete (fmap getNodeId e) $ edgeMap g
      , nodeMap = nodeMap g
      }
  removeNode n g =
    AlgaGraph
      { adjacencyMap = G.removeVertex (getNodeId n) $ adjacencyMap g
      , edgeMap = foldl' (flip HMap.delete) (edgeMap g) edgesToRemove
      , nodeMap = HMap.delete (getNodeId n) $ nodeMap g
      }
   where
    edgesToRemove :: [Edge (NodeId i)]
    edgesToRemove =
      fmap getNodeId
        <$> (Edge n <$> HSet.toList (succs n g))
        ++ ((`Edge` n) <$> HSet.toList (preds n g))

  addNodes ns g = AlgaGraph
  -- Adding nodes that are already in the graph should end up having no effect.
  -- The node IDs are used to reference those nodes in the adjacencyMap and nodeMap,
  -- and there will still be a single entry for each node.
    { adjacencyMap = G.overlay (adjacencyMap g)
                     . G.vertices
                     . fmap getNodeId
                     $ ns
    , edgeMap = edgeMap g
    , nodeMap = HMap.union (nodeMap g)
                $ HMap.fromList ((getNodeId &&& identity) <$> ns)
    }

  addEdge (LEdge lbl e) g =
    AlgaGraph
      { adjacencyMap =
          G.overlay (adjacencyMap g) $
            G.edge (getNodeId (e ^. #src)) (getNodeId (e ^. #dst))
      , edgeMap = HMap.insert (getNodeId <$> e) lbl $ edgeMap g
      , nodeMap =
          HMap.insert dstId (e ^. #dst)
            . HMap.insert srcId (e ^. #src)
            $ nodeMap g
      }
   where
    srcId :: NodeId i
    dstId :: NodeId i
    (srcId, dstId) = toTupleEdge $ getNodeId <$> e

  hasNode n = G.hasVertex (getNodeId n) . adjacencyMap
  updateNode f n = over #nodeMap (HMap.alter (fmap f) (getNodeId n))
  transpose g = g { adjacencyMap = G.transpose $ adjacencyMap g
                  , edgeMap = HMap.mapKeys (\(Edge a b) -> Edge b a) $ edgeMap g
                  }
  bfs startNodes g = mapMaybe (getNode g) <$> GA.bfs (fmap getNodeId startNodes) (adjacencyMap g)
  subgraph pred g = AlgaGraph
    { adjacencyMap = subgraphAdjMap
    , edgeMap = (`HMap.filterWithKey` edgeMap g) $ \k _ ->
        Set.member k subgraphEdges
    , nodeMap = flip HMap.mapMaybeWithKey (nodeMap g) $ \k v ->
        if pred' k then Just v else Nothing
    }
      where
        pred' :: NodeId i -> Bool
        pred' nid = maybe False pred $ HMap.lookup nid (nodeMap g)
        subgraphAdjMap :: G.AdjacencyMap (NodeId i)
        subgraphAdjMap = G.induce pred' (adjacencyMap g)
        subgraphEdges :: Set (Edge (NodeId i))
        subgraphEdges = Set.fromList . fmap (uncurry Edge) $ G.edgeList subgraphAdjMap

  reachable n g = mapMaybe (getNode g) $ GA.reachable (getNodeId n) (adjacencyMap g)

getNode :: Hashable i => AlgaGraph l i n -> NodeId i -> Maybe n
getNode graph nodeId = HMap.lookup nodeId (nodeMap graph)

-- | Exports the graph to a DOT representation.
toDot :: (Ord i, Hashable i) => (n -> Text) -> AlgaGraph l i n -> Text
toDot nodeToText g =
  Dot.export (Dot.defaultStyle (nodeToText . fromJust . (`HMap.lookup` nodeMap g))) (adjacencyMap g)

isAcyclic :: Ord i => AlgaGraph l i n -> Bool
isAcyclic = GA.isAcyclic . adjacencyMap
