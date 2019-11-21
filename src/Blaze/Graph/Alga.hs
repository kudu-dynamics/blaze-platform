module Blaze.Graph.Alga where

import Blaze.Prelude

import qualified Algebra.Graph.AdjacencyMap as G
import qualified Data.Set as Set
import qualified Data.Map as Map
import Blaze.Types.Path (Node, PathGraph)
import Blaze.Types.Graph
import qualified Algebra.Graph.Export.Dot as Dot

type AlgaPath = PathGraph (AlgaGraph () Node)

data AlgaGraph e a = AlgaGraph
  { adjacencyMap :: G.AdjacencyMap a
  , edgeMap :: Map (a, a) e
  }

instance (Ord e, Ord n) => Graph e n (AlgaGraph e n) where
  empty = AlgaGraph G.empty Map.empty
  fromNode = flip AlgaGraph Map.empty . G.vertex
  fromEdges ledges = AlgaGraph
    { adjacencyMap = G.edges . map snd $ ledges
    , edgeMap = Map.fromList . fmap swap $ ledges
    }
  succs n g = G.postSet n . adjacencyMap $ g
  preds n g = G.preSet n . adjacencyMap $ g
  nodes = Set.fromList . G.vertexList . adjacencyMap
  edges g = catMaybes . fmap (\p -> (,p) <$> Map.lookup p (edgeMap g)) . G.edgeList . adjacencyMap $ g
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
  addEdge (e, (n1, n2)) g = AlgaGraph
    { adjacencyMap = G.overlay (adjacencyMap g) $ G.edge n1 n2
    , edgeMap = Map.insert (n1, n2) e $ edgeMap g
    }

bill :: AlgaGraph () Int
bill = fromEdges . fmap ((),) $
  [ (1, 2)
  , (2, 3)
--  , (3, 1)
  , (3, 8)
  , (3, 4)
  , (2, 4)
  , (4, 5)]

-- instance (Ord a, Ord e) => Graph (AlgaGraph e a) e a where
--   fromEdges ledges = AlgaGraph
--     { adjacencyMap = G.edges . map snd $ ledges
--     , edgeMap = Bimap.fromList ledges
--     }

--   succs n g = G.postSet n . adjacencyMap $ g

--   preds n g = G.preSet n . adjacencyMap $ g

--   getEdgeLabel edge = Bimap.lookupR edge . edgeMap

--   setEdgeLabel label edge g = g { edgeMap = Bimap.insert label edge $ edgeMap g }




  
-- instance (Ord a, Ord e) => Graph (AlgaGraph e a) where
--   type EdgeLabel (AlgaGraph e a) = e
--   type Node (AlgaGraph e a) = a

--   fromEdges ledges = AlgaGraph
--     { adjacencyMap = G.edges . map snd $ ledges
--     , edgeMap = Bimap.fromList ledges
--     }

--   succs n g = G.postSet n . adjacencyMap $ g

--   preds n g = G.preSet n . adjacencyMap $ g

--   getEdgeLabel edge = Bimap.lookupR edge . edgeMap

--   setEdgeLabel label edge g = g { edgeMap = Bimap.insert label edge $ edgeMap g }

-- instance EmptyGraph (AlgaGraph e n) where
--   empty = AlgaGraph G.empty Bimap.empty
  
-- instance Ord n => BasicGraph n (AlgaGraph e n) where
--   fromNode = flip AlgaGraph Bimap.empty . G.vertex
--   succs n g = G.postSet n . adjacencyMap $ g

--   preds n g = G.preSet n . adjacencyMap $ g

  
-- instance (Ord e, Ord n) => EdgeGraph e n (AlgaGraph e n) where
--   fromEdges ledges = AlgaGraph
--     { adjacencyMap = G.edges . map snd $ ledges
--     , edgeMap = Bimap.fromList ledges
--     }

--   getEdgeLabel edge = Bimap.lookupR edge . edgeMap

--   setEdgeLabel label edge g = g { edgeMap = Bimap.insert label edge $ edgeMap g }

demograph :: AlgaGraph () Char
demograph = fromEdges . fmap ((),) $ [ ('z', 'a')
                                     , ('a', 'b')
                                     , ('a', 'c')
                                     , ('d', 'c')
                                     , ('b', 'g')
                                     , ('b', 'f')
                                     , ('c', 'e')
                                     ]


toDot :: Ord n => (n -> Text) -> AlgaGraph e n -> Text
toDot nodeToText g = Dot.export (Dot.defaultStyle nodeToText) (adjacencyMap g)
