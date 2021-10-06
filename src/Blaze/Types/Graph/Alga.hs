module Blaze.Types.Graph.Alga where

import Blaze.Prelude hiding (pred)

import qualified Algebra.Graph.AdjacencyMap as G
import qualified Algebra.Graph.AdjacencyMap.Algorithm as GA
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet
import Blaze.Types.Graph hiding (edge, label, src, dst)
import qualified Algebra.Graph.Export.Dot as Dot

data AlgaGraph e attr n = AlgaGraph
  { adjacencyMap :: G.AdjacencyMap n
  , edgeMap :: HashMap (Edge n) e
  , nodeAttrMap :: HashMap n attr
  } deriving (Generic, Show, Ord, Eq)

-- TODO: see if G.AdjacencyMap's Eq is good enough
-- I think that two graphs with identitcal nodes and edges will not be equal

instance (NFData e, NFData n, NFData attr) => NFData (AlgaGraph e attr n)

instance (Ord n, Hashable n) => Graph e attr n (AlgaGraph e attr n) where
  empty = AlgaGraph G.empty HMap.empty HMap.empty
  fromNode node = AlgaGraph (G.vertex node) HMap.empty HMap.empty
  fromEdges ledges = AlgaGraph
    { adjacencyMap = G.edges . fmap (toTupleEdge . view #edge) $ ledges
    , edgeMap = HMap.fromList $ (\e -> (e ^. #edge, e ^. #label)) <$> ledges
    , nodeAttrMap = HMap.empty
    }
  succs n = HSet.fromList . Set.toList . G.postSet n . adjacencyMap
  preds n = HSet.fromList . Set.toList . G.preSet n . adjacencyMap
  nodes = HSet.fromList . G.vertexList . adjacencyMap
  edges g = mapMaybe (f . fromTupleEdge) . G.edgeList . adjacencyMap $ g
    where
      f e = flip LEdge e <$> HMap.lookup e (edgeMap g)

  getEdgeLabel edge = HMap.lookup edge . edgeMap
  setEdgeLabel label edge g = g { edgeMap = HMap.insert edge label $ edgeMap g }

  getNodeAttr node = HMap.lookup node . nodeAttrMap
  setNodeAttr attr node g = g & #nodeAttrMap %~ HMap.insert node attr
  getNodeAttrMap g = g ^. #nodeAttrMap

  removeEdge e@(Edge n1 n2) g = AlgaGraph
    { adjacencyMap = G.removeEdge n1 n2 $ adjacencyMap g
    , edgeMap = HMap.delete e $ edgeMap g
    , nodeAttrMap = nodeAttrMap g
    }
  removeNode n g = AlgaGraph
    { adjacencyMap = G.removeVertex n $ adjacencyMap g
    , edgeMap = foldl' (flip HMap.delete) (edgeMap g) edgesToRemove
    , nodeAttrMap = HMap.delete n $ nodeAttrMap g
    }
      where
        edgesToRemove :: [Edge n]
        edgesToRemove = (Edge n <$> HSet.toList (succs n g)) 
          ++ ((`Edge` n) <$> HSet.toList (preds n g))

  addNodes ns g = AlgaGraph
    { adjacencyMap = G.overlay (adjacencyMap g)
                     . G.vertices
                     $ ns
    , edgeMap = edgeMap g
    , nodeAttrMap = nodeAttrMap g
    }

  addNodesWithAttrs ns g = AlgaGraph
    { adjacencyMap = G.overlay (adjacencyMap g)
                     . G.vertices
                     . fmap fst
                     $ ns
    , edgeMap = edgeMap g
    , nodeAttrMap = HMap.union (HMap.fromList ns) $ nodeAttrMap g
    }
 
  addEdge (LEdge lbl e) g = AlgaGraph
    { adjacencyMap = G.overlay (adjacencyMap g) $ G.edge (e ^. #src) (e ^. #dst)
    , edgeMap = HMap.insert e lbl $ edgeMap g
    , nodeAttrMap = nodeAttrMap g
    }
  hasNode n = G.hasVertex n . adjacencyMap
  transpose g = g {adjacencyMap = G.transpose $ adjacencyMap g}
  bfs startNodes g = GA.bfs startNodes . adjacencyMap $ g
  subgraph pred g = AlgaGraph 
    { adjacencyMap = subgraphAdjMap
    , edgeMap = (`HMap.filterWithKey` edgeMap g) $ \k _ ->
        Set.member k subgraphEdges
    , nodeAttrMap = flip HMap.mapMaybeWithKey (nodeAttrMap g) $ \k v ->
        if pred k then Just v else Nothing
    }
      where
        subgraphAdjMap :: G.AdjacencyMap n
        subgraphAdjMap = G.induce pred (adjacencyMap g)
        subgraphEdges :: Set (Edge n)
        subgraphEdges = Set.fromList . fmap (uncurry Edge) $ G.edgeList subgraphAdjMap

  reachable n g = GA.reachable n (adjacencyMap g)

toDot :: Ord n => (n -> Text) -> AlgaGraph e attr n -> Text
toDot nodeToText g = Dot.export (Dot.defaultStyle nodeToText) (adjacencyMap g)

isAcyclic :: Ord n => AlgaGraph e attr n -> Bool
isAcyclic = GA.isAcyclic . adjacencyMap
