{-# LANGUAGE TemplateHaskell #-}

module Blaze.Util.Graph where

import Blaze.Prelude

import qualified Algebra.Graph.AdjacencyMap as G
import qualified Data.Set as Set

type Graph a = G.AdjacencyMap a

demoGraph :: Graph Int
demoGraph = 1 * 2
          + 2 * (3 + 4)
          + 3 * (6 + 8 + 5)
          + 4 * (2 + 5 + 9)
          + 6 * 7
          + 7 * 5

findSimplePaths' :: Ord a => Set a -> a -> a -> Graph a -> [[a]]
findSimplePaths' seen start' end' g = fmap (start':) $ do
  succ' <- Set.toList $ G.postSet start' g `Set.difference` seen
  if succ' == end'
    then return [succ']
    else findSimplePaths' (Set.insert succ' seen) succ' end' g
  
--- simple paths (non-repeating) from start to end
findSimplePaths :: Ord a => a -> a -> Graph a -> [[a]]
findSimplePaths = findSimplePaths' Set.empty

sources :: Ord a => Graph a -> [a]
sources g = filter ((== 0) . Set.size . flip G.preSet g) $ G.vertexList g

sinks :: Ord a => Graph a -> [a]
sinks g = filter ((== 0) . Set.size . flip G.postSet g) $ G.vertexList g

