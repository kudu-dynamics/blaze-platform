module Blaze.Types.Graph.Alga where

import Blaze.Prelude hiding (pred)

import qualified Algebra.Graph.AdjacencyMap as G
import qualified Algebra.Graph.AdjacencyMap.Algorithm as GA
import qualified Data.Set as Set
import qualified Data.Map as Map
import Blaze.Types.Graph
import qualified Algebra.Graph.Export.Dot as Dot

data AlgaGraph e a = AlgaGraph
  { adjacencyMap :: G.AdjacencyMap a
  , edgeMap :: Map (a, a) e
  } deriving (Generic, Show, Ord, Eq)

-- TODO: see if G.AdjacencyMap's Eq is good enough
-- I think that two graphs with identitcal nodes and edges will not be equal

instance (NFData e, NFData a) => NFData (AlgaGraph e a)

instance (Ord n) => Graph e n (AlgaGraph e n) where
  empty = AlgaGraph G.empty Map.empty
  fromNode = flip AlgaGraph Map.empty . G.vertex
  fromEdges ledges = AlgaGraph
    { adjacencyMap = G.edges . map snd $ ledges
    , edgeMap = Map.fromList . fmap swap $ ledges
    }
  succs n = G.postSet n . adjacencyMap
  preds n = G.preSet n . adjacencyMap
  nodes = Set.fromList . G.vertexList . adjacencyMap
  edges g = mapMaybe (\p -> (,p) <$> Map.lookup p (edgeMap g)) . G.edgeList . adjacencyMap $ g
  getEdgeLabel edge = Map.lookup edge . edgeMap
  setEdgeLabel label edge g = g { edgeMap = Map.insert edge label $ edgeMap g }
  removeEdge e@(n1, n2) g = AlgaGraph
    { adjacencyMap = G.removeEdge n1 n2 $ adjacencyMap g
    , edgeMap = Map.delete e $ edgeMap g
    }
  removeNode n g = AlgaGraph
    { adjacencyMap = G.removeVertex n $ adjacencyMap g
    , edgeMap = Map.filterWithKey (\(n1, n2) _ -> n1 == n || n2 == n) $ edgeMap g
    }
  addNodes ns g = AlgaGraph
    { adjacencyMap = G.overlay (adjacencyMap g) $ G.vertices ns
    , edgeMap = edgeMap g}
  addEdge (e, (n1, n2)) g = AlgaGraph
    { adjacencyMap = G.overlay (adjacencyMap g) $ G.edge n1 n2
    , edgeMap = Map.insert (n1, n2) e $ edgeMap g
    }
  hasNode n = G.hasVertex n . adjacencyMap
  transpose g = g {adjacencyMap = G.transpose $ adjacencyMap g}
  bfs startNodes g = GA.bfs startNodes . adjacencyMap $ g
  subgraph pred g = AlgaGraph 
    { adjacencyMap = subgraphAdjMap
    , edgeMap = Map.restrictKeys (edgeMap g) subgraphEdges
    }
      where
        subgraphAdjMap :: G.AdjacencyMap n
        subgraphAdjMap = G.induce pred (adjacencyMap g)
        subgraphEdges :: Set (n, n)
        subgraphEdges = Set.fromList $ G.edgeList subgraphAdjMap

toDot :: Ord n => (n -> Text) -> AlgaGraph e n -> Text
toDot nodeToText g = Dot.export (Dot.defaultStyle nodeToText) (adjacencyMap g)

isAcyclic :: Ord n => AlgaGraph e n -> Bool
isAcyclic = GA.isAcyclic . adjacencyMap

reachable :: Ord n => n -> AlgaGraph e n -> [n]
reachable x = GA.reachable x . adjacencyMap

