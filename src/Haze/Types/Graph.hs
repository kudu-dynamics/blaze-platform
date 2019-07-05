module Haze.Types.Graph where

import Haze.Prelude

import qualified Algebra.Graph.AdjacencyMap as G
import qualified Data.Set as Set

type LEdge label node = (label, (node, node))

type Edge node = (node, node)


-- class Graph g where
--   type EdgeLabel g :: *
--   type Node g :: *
--   fromEdges :: [LEdge (EdgeLabel g) (Node g)] -> g
--   succs :: Node g -> g -> Set (Node g)
--   preds :: Node g -> g -> Set (Node g)
--   getEdgeLabel :: Edge (Node g) -> g -> Maybe (EdgeLabel g)
--   setEdgeLabel :: EdgeLabel g -> Edge (Node g) -> g -> g


-- findSimplePaths' :: (Graph g, Ord (Node g)) => Set (Node g) -> (Node g) -> (Node g) ->g -> [[Node g]]
-- findSimplePaths' seen start' end' g = fmap (start':) $ do
--   succ' <- Set.toList $ succs start' g `Set.difference` seen
--   if succ' == end'
--     then return [succ']
--     else findSimplePaths' (Set.insert succ' seen) succ' end' g
  
-- --- simple paths (non-repeating) from start to end
-- findSimplePaths :: (Graph g, Ord (Node g)) => Node g -> Node g -> g -> [[Node g]]
-- findSimplePaths = findSimplePaths' Set.empty

--------------------------
--------------------------
class GMapKey k t v where
  data GMap k t v :: *
  empty       :: GMap k t v
  lookup      :: k -> GMap k t v -> Maybe v
  insert      :: k -> v -> GMap k t v -> GMap k t v

class (BasicGraph node g, EdgeGraph edge node g) => Graph edge node g

class BasicGraph node g where
  succs :: node -> g -> Set node
  preds :: node -> g -> Set node

class EdgeGraph edge node g where
  fromEdges :: [(edge, (node, node))] -> g  
  getEdgeLabel :: (node, node) -> g -> Maybe edge
  setEdgeLabel :: edge -> (node, node) -> g -> g

findSimplePaths' :: (BasicGraph n g, Ord n) => Set n -> n -> n -> g -> [[n]]
findSimplePaths' seen start' end' g = fmap (start':) $ do
  succ' <- Set.toList $ succs start' g `Set.difference` seen
  if succ' == end'
    then return [succ']
    else findSimplePaths' (Set.insert succ' seen) succ' end' g
  
--- simple paths (non-repeating) from start to end
findSimplePaths :: (BasicGraph n g, Ord n) => n -> n -> g -> [[n]]
findSimplePaths = findSimplePaths' Set.empty

--------------------------
--------------------------

-- class Graph g e n | g -> e n where
--   fromEdges :: [LEdge e n] -> g
--   succs :: n -> g -> Set n
--   preds :: n -> g -> Set n
--   getEdgeLabel :: Edge n -> g -> Maybe e
--   setEdgeLabel :: e -> Edge a -> g -> g

-- findSimplePaths' :: (Graph g e n, Ord n) => Set n -> n -> n -> g -> [[n]]
-- findSimplePaths' seen start' end' g = fmap (start':) $ do
--   succ' <- Set.toList $ succs start' g `Set.difference` seen
--   if succ' == end'
--     then return [succ']
--     else findSimplePaths' (Set.insert succ' seen) succ' end' g

-- findSimplePaths :: (Graph g edge node, Ord node) => node -> node -> g -> [[node]]
-- findSimplePaths = findSimplePaths' Set.empty

