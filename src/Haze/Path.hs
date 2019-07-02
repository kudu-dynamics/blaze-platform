{-# LANGUAGE TemplateHaskell #-}

module Haze.Path where

import Haze.Prelude

import qualified Hinja.Core as H
import Hinja.Function (Function, MLILFunction)
import Haze.Types.Path
import Hinja.BasicBlock (BasicBlock, BlockEdge, BasicBlockFunction)
import qualified Hinja.BasicBlock as BB
import qualified Algebra.Graph.AdjacencyMap as G
import qualified Data.Set as Set

type Graph a = G.AdjacencyMap a

type BasicBlockGraph t = G.AdjacencyMap (BasicBlock t)

constructBasicBlockGraph :: (Ord t, BasicBlockFunction t)
                         => t -> IO (BasicBlockGraph t)
constructBasicBlockGraph fn = do
  bbs <- BB.getBasicBlocks fn
  succs <- traverse cleanSuccs bbs
  return . G.edges $ succsToEdges succs
  where
    cleanSuccs :: BasicBlockFunction t => BasicBlock t -> IO (BasicBlock t, [BasicBlock t])
    cleanSuccs bb = (bb,) . catMaybes . fmap (view BB.target)
                    <$> BB.getOutgoingEdges bb
    succsToEdges :: [(a, [a])] -> [(a, a)]
    succsToEdges xs = do
      (x, ys) <- xs
      y <- ys
      return (x, y)

demoGraph :: Graph Int
demoGraph = 1 * 2
          + 2 * (3 + 4)
          + 3 * (6 + 8 + 5)
          + 4 * (2 + 5 + 9)
          + 6 * 7
          + 7 * 5


findSimplePaths' :: Ord a => Set a -> a -> a -> Graph a -> [[a]]
findSimplePaths' seen start' end' g = do
  succ' <- Set.toList $ G.postSet start' g `Set.difference` seen
  if succ' == end'
    then return [succ']
    else do found <- findSimplePaths' (Set.insert succ' seen) succ' end' g
            return (start':found)
  

-- simple paths (non-repeating) from start to end
-- findSimplePaths :: Ord a => a -> a -> Graph a -> [[a]]
-- findSimplePaths start end g = 


