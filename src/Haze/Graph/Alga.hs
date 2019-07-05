module Haze.Graph.Alga where

import Haze.Prelude

import qualified Algebra.Graph.AdjacencyMap as G
--import qualified Data.Set as Set
import qualified Data.Bimap as Bimap
import Data.Bimap (Bimap)
import Haze.Types.Graph

data AlgaGraph e a = AlgaGraph
  { adjacencyMap :: G.AdjacencyMap a
  , edgeMap :: Bimap e (a, a)
  }

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
  
instance Ord n => BasicGraph n (AlgaGraph e n) where
  succs n g = G.postSet n . adjacencyMap $ g

  preds n g = G.preSet n . adjacencyMap $ g

  
instance (Ord e, Ord n) => EdgeGraph e n (AlgaGraph e n) where
  fromEdges ledges = AlgaGraph
    { adjacencyMap = G.edges . map snd $ ledges
    , edgeMap = Bimap.fromList ledges
    }

  getEdgeLabel edge = Bimap.lookupR edge . edgeMap

  setEdgeLabel label edge g = g { edgeMap = Bimap.insert label edge $ edgeMap g }
  
