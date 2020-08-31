module Blaze.Types.Graph where

import Blaze.Prelude

import qualified Prelude as P
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy ((!))

type LEdge label node = (label, (node, node))

type Edge node = (node, node)

class Graph e n g | g -> e n where
  empty :: g
  fromNode :: n -> g
  fromEdges :: [(e, (n, n))] -> g
  succs :: n -> g -> Set n
  preds :: n -> g -> Set n
  nodes :: g -> Set n
  edges :: g -> [(e, (n, n))]
  getEdgeLabel :: (n, n) -> g -> Maybe e
  setEdgeLabel :: e -> (n, n) -> g -> g
  removeEdge :: (n, n) -> g -> g
  removeNode :: n -> g -> g
  addNodes :: [n] -> g -> g
  addEdge :: (e, (n, n)) -> g -> g
  hasNode :: n -> g -> Bool
  transpose :: g -> g
  bfs :: [n] -> g -> [[n]]
  subgraph :: (n -> Bool) -> g -> g

-- findNonRepeatPaths' :: (Graph e n g, Ord n) => Set n -> n -> g -> [[n]]
-- findNonRepeatPaths' seen start' g = fmap (start':) $ do
--   succ' <- Set.toList $ succs start' g `Set.difference` seen
--   if succ' == end'
--     then return [succ']
--     else findNonRepeatPaths' (Set.insert succ' seen) succ' end' g

-- findNonRepeatPaths' :: (Graph e n g, Ord n) => Set n -> n -> g -> [[n]]
-- findNonRepeatPaths' seen start' g = fmap (start':) $ do
--   succ' <- Set.toList $ succs start' g `Set.difference` seen
--   P.error $ show . length $ findNonRepeatPaths' (Set.insert succ' seen) succ' g
--   return [succ']


findNonRepeatPaths' :: (Graph e n g, Ord n) => Set n -> n -> g -> [[n]]
findNonRepeatPaths' seen start' g = case ((start' :) <$> succsPaths) of
  [] -> [[start']]
  xs -> xs
  where
    succs' = Set.toList $ succs start' g `Set.difference` seen

    succsPaths = concatMap (\s -> findNonRepeatPaths' (Set.insert s seen) s g) succs'

findNonRepeatPaths :: (Graph e n g, Ord n) => n -> g -> [[n]]
findNonRepeatPaths start' = findNonRepeatPaths' (Set.singleton start') start'

-- | finds all paths up until a repeat or a node with no succs
findAllNonRepeatPaths :: (Graph e node g, Ord node) => g -> [[node]]
findAllNonRepeatPaths g 
  | length (nodes g) == 1 = [Set.toList $ nodes g]
  | otherwise = do
      src <- Set.toList $ sources g
      findNonRepeatPaths src g

findSimplePaths' :: (Graph e n g, Ord n) => Set n -> n -> n -> g -> [[n]]
findSimplePaths' seen start' end' g = fmap (start':) $ do
  succ' <- Set.toList $ succs start' g `Set.difference` seen
  if succ' == end'
    then return [succ']
    else findSimplePaths' (Set.insert succ' seen) succ' end' g

findSimplePaths :: (Graph e node g, Ord node) => node -> node -> g -> [[node]]
findSimplePaths = findSimplePaths' Set.empty

findAllSimplePaths :: (Graph e node g, Ord node) => g -> [[node]]
findAllSimplePaths g 
  | length (nodes g) == 1 = [Set.toList $ nodes g]
  | otherwise = do
      src <- Set.toList $ sources g
      sink <- Set.toList $ sinks g
      findSimplePaths src sink g

sources :: Graph e n g => g -> Set n
sources g = Set.filter ((== 0) . Set.size . flip preds g) . nodes $ g

sinks :: Graph e n g => g -> Set n
sinks g = Set.filter ((== 0) . Set.size . flip succs g) . nodes $ g

removeEdges :: Graph e n g => [(n, n)] -> g -> g
removeEdges = flip $ foldr removeEdge

addEdges :: Graph e n g => [(e, (n, n))] -> g -> g
addEdges = flip $ foldr addEdge

reverseSpan :: Graph e n g => g -> Int -> n -> [[n]]
reverseSpan _ 0 node = [[node]]
reverseSpan g depth node = case Set.toList $ preds node g of
  [] -> [[node]]
  xs -> fmap (node:) . concatMap (reverseSpan g (depth - 1)) $ xs
  

findAllSimplePaths2 :: forall e node g. (Graph e node g, Ord node)
                    => g -> node -> [[node]]
findAllSimplePaths2 g startNode =
  let m = mkNonLoopingNodeMap m (Set.toList $ nodes g) in
    m ! startNode
  where
    mkNonLoopingNodeMap :: Map node [[node]] -> [node] -> Map node [[node]]
    mkNonLoopingNodeMap m ns = Map.fromList $ do
      n <- ns
      let succPaths = concatMap (\s -> case m ! s of
                                        [] -> [[s]]
                                        xs -> (s:) <$> xs)
                      . Set.toList $ succs n g
      return (n, succPaths)


countAllSimplePaths :: forall e node g. (Graph e node g, Ord node)
                    => g -> Map node Integer
countAllSimplePaths g =
  let m = mkNonLoopingNodeMap m (Set.toList $ nodes g) in
    m
  where
    mkNonLoopingNodeMap :: Map node Integer -> [node] -> Map node Integer
    mkNonLoopingNodeMap m ns = Map.fromList $ do
      n <- ns
      let ss = Set.toList $ succs n g
      let x = case ss of
            [] -> 1
            xs -> sum . fmap (m !) $ xs
      return (n, x)

maxSimplePaths :: forall e node g. (Graph e node g, Ord node)
               => g -> Integer
maxSimplePaths = foldr max 0 . countAllSimplePaths

-- -- The total number of 
-- descendentFrequencyCount :: forall e node g. (Graph e node g, Ord node)
--                     => g -> Map node (Map node Int)
-- descendentFrequencyCount g =
--   let m = mkNonLoopingNodeMap m (Set.toList $ nodes g) in
--     m
--   where
--     mkNonLoopingNodeMap :: Map node Integer -> [node] -> Map node Integer
--     mkNonLoopingNodeMap m ns = Map.fromList $ do
--       n <- ns
--       let ss = Set.toList $ succs n g
--       let x = case ss of
--             [] -> 1
--             xs -> foldr (+) 0 . fmap (m !) $ xs
--       return (n, x)

newtype DescendentsMap node = DescendentsMap (Map node (Set node))
  deriving (Eq, Ord, Show)

calcDescendentsMap :: forall e node g. (Graph e node g, Ord node)
            => g -> DescendentsMap node
calcDescendentsMap g =
  let m = mkNonLoopingNodeMap m (Set.toList $ nodes g) in
    DescendentsMap m
  where
    mkNonLoopingNodeMap :: Map node (Set node) -> [node] -> Map node (Set node)
    mkNonLoopingNodeMap m ns = Map.fromList $ do
      n <- ns
      let ss = Set.toList $ succs n g
      let x = case ss of
            [] -> Set.empty
            xs -> foldr Set.union Set.empty . fmap (\s -> Set.insert s $ m ! s) $ xs
      return (n, x)

-- assumes DescendentMap contains start node and is derived from g...
searchBetween_ :: forall e node g. (Graph e node g, Ord node)
              => g -> DescendentsMap node -> node -> node -> [[node]]
searchBetween_ g (DescendentsMap dm) start end
  | start == end = return [end]
  | Set.member end (dm ! start) = do
      kid <- Set.toList $ succs start g
      kidPath <- searchBetween_ g (DescendentsMap dm) kid end
      return $ start : kidPath      
  | otherwise = []

{- HLINT ignore searchBetween "Eta reduce" -}
searchBetween :: forall e node g. (Graph e node g, Ord node)
              => g -> node -> node -> [[node]]
searchBetween g start end = searchBetween_ g (calcDescendentsMap g) start end


-- -- finding parents is (n * log(n)) for each node
-- -- this 
-- parentMap :: forall e node g. (Graph e node g, Ord node)
--           => g -> HashMap n (HashSet n)
          

siblings :: forall e node g. (Graph e node g, Ord node)
         => node -> node -> g -> Set node
siblings child parent g = Set.delete child $ succs parent g

mapGraph :: (Graph e n g, Graph e' n' g')
         => (e -> e') -> (n -> n') -> g -> g'
mapGraph ef nf = fromEdges . fmap (\ (e, (n1, n2)) -> (ef e, (nf n1, nf n2))) . edges


mapEdges :: (Graph e n g, Graph e' n g')
         => (e -> e') -> g -> g'
mapEdges f = mapGraph f identity


mapNodes :: (Graph e n g, Graph e n' g') => (n -> n') -> g -> g'
mapNodes = mapGraph identity


    
