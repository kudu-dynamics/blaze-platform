module Blaze.Types.Graph.Alga where

import Blaze.Prelude hiding (pred)

import qualified Algebra.Graph.AdjacencyMap as G
import qualified Algebra.Graph.AdjacencyMap.Algorithm as GA
import qualified Data.Set as Set
import qualified Data.Map as Map
import Blaze.Types.Graph hiding (edge, label, src, dst)
import qualified Algebra.Graph.Export.Dot as Dot

data AlgaGraph e attr n = AlgaGraph
  { adjacencyMap :: G.AdjacencyMap n
  , edgeMap :: Map (Edge n) e
  , nodeAttrMap :: Map n attr
  } deriving (Generic, Show, Ord, Eq)

-- TODO: see if G.AdjacencyMap's Eq is good enough
-- I think that two graphs with identitcal nodes and edges will not be equal

instance (NFData e, NFData n, NFData attr) => NFData (AlgaGraph e attr n)

instance (Ord n) => Graph e attr n (AlgaGraph e attr n) where
  empty = AlgaGraph G.empty Map.empty Map.empty
  fromNode node = AlgaGraph (G.vertex node) Map.empty Map.empty
  fromEdges ledges = AlgaGraph
    { adjacencyMap = G.edges . fmap (toTupleEdge . view #edge) $ ledges
    , edgeMap = Map.fromList $ (\e -> (e ^. #edge, e ^. #label)) <$> ledges
    , nodeAttrMap = Map.empty
    }
  succs n = G.postSet n . adjacencyMap
  preds n = G.preSet n . adjacencyMap
  nodes = Set.fromList . G.vertexList . adjacencyMap
  edges g = mapMaybe (f . fromTupleEdge) . G.edgeList . adjacencyMap $ g
    where
      f e = LEdge e <$> Map.lookup e (edgeMap g)

  getEdgeLabel edge = Map.lookup edge . edgeMap
  setEdgeLabel label edge g = g { edgeMap = Map.insert edge label $ edgeMap g }


  getNodeAttr node = Map.lookup node . nodeAttrMap
  setNodeAttr attr node g = g & #nodeAttrMap %~ Map.insert node attr

  removeEdge e@(Edge n1 n2) g = AlgaGraph
    { adjacencyMap = G.removeEdge n1 n2 $ adjacencyMap g
    , edgeMap = Map.delete e $ edgeMap g
    , nodeAttrMap = nodeAttrMap g
    }
  removeNode n g = AlgaGraph
    { adjacencyMap = G.removeVertex n $ adjacencyMap g
    , edgeMap = foldl' (flip Map.delete) (edgeMap g) edgesToRemove
    , nodeAttrMap = Map.delete n $ nodeAttrMap g
    }
      where
        edgesToRemove :: [Edge n]
        edgesToRemove = (Edge n <$> Set.toList (succs n g)) 
          ++ ((`Edge` n) <$> Set.toList (preds n g))

  addNodes ns g = AlgaGraph
    { adjacencyMap = G.overlay (adjacencyMap g) $ G.vertices ns
    , edgeMap = edgeMap g
    , nodeAttrMap = nodeAttrMap g
    }

  addEdge (LEdge e lbl) g = AlgaGraph
    { adjacencyMap = G.overlay (adjacencyMap g) $ G.edge (e ^. #src) (e ^. #dst)
    , edgeMap = Map.insert e lbl $ edgeMap g
    , nodeAttrMap = nodeAttrMap g
    }
  hasNode n = G.hasVertex n . adjacencyMap
  transpose g = g {adjacencyMap = G.transpose $ adjacencyMap g}
  bfs startNodes g = GA.bfs startNodes . adjacencyMap $ g
  subgraph pred g = AlgaGraph 
    { adjacencyMap = subgraphAdjMap
    , edgeMap = Map.restrictKeys (edgeMap g) subgraphEdges
    , nodeAttrMap = flip Map.mapMaybeWithKey (nodeAttrMap g) $ \k v ->
        if pred k then Just v else Nothing
    }
      where
        subgraphAdjMap :: G.AdjacencyMap n
        subgraphAdjMap = G.induce pred (adjacencyMap g)
        subgraphEdges :: Set (Edge n)
        subgraphEdges = Set.fromList . fmap (uncurry Edge) $ G.edgeList subgraphAdjMap

toDot :: Ord n => (n -> Text) -> AlgaGraph e attr n -> Text
toDot nodeToText g = Dot.export (Dot.defaultStyle nodeToText) (adjacencyMap g)

isAcyclic :: Ord n => AlgaGraph e attr n -> Bool
isAcyclic = GA.isAcyclic . adjacencyMap

reachable :: Ord n => n -> AlgaGraph e attr n -> [n]
reachable x = GA.reachable x . adjacencyMap

