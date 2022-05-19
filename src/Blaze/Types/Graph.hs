{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Blaze.Types.Graph where

import Blaze.Prelude hiding (transpose)

import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet

-- | An integer node ID.
newtype NodeId a = NodeId a
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable, NFData, ToJSON, FromJSON)

data Edge node = Edge
  { src :: node
  , dst :: node
  } deriving (Eq, Ord, Show, Generic, NFData, Functor, Foldable, Traversable)
  deriving anyclass (Hashable, ToJSON, FromJSON)

data LEdge label node = LEdge
  { label :: label
  , edge :: Edge node
  } deriving (Eq, Ord, Show, Generic, NFData, Functor, Foldable, Traversable)
  deriving anyclass (Hashable, ToJSON, FromJSON)

toTupleEdge :: Edge node -> (node, node)
toTupleEdge e = (e ^. #src, e ^. #dst)

fromTupleEdge :: (node, node) -> Edge node
fromTupleEdge (a, b) = Edge a b

toTupleLEdge :: LEdge label node -> (label, (node, node))
toTupleLEdge (LEdge lbl e) = (lbl, toTupleEdge e)

fromTupleLEdge :: (label, (node, node)) -> LEdge label node
fromTupleLEdge (lbl, e) = LEdge lbl (fromTupleEdge e)

nodesFromEdges :: Hashable n => [LEdge l n] -> HashSet n
nodesFromEdges es = HSet.fromList $ concatMap (\(LEdge _ e) -> e ^. #src : [e ^. #dst]) es

class
  ( Functor g
  , Foldable g
  , Traversable g
  ) =>
  Graph l n g | g -> l where

  empty :: g n
  fromNode :: n -> g n
  fromEdges :: [LEdge l n] -> g n
  succs :: Hashable n => n -> g n -> HashSet n
  preds :: Hashable n => n -> g n -> HashSet n
  nodes :: Hashable n => g n -> HashSet n
  edges :: g n -> [LEdge l n]
  getEdgeLabel :: Edge n -> g n -> Maybe l
  setEdgeLabel :: l -> Edge n -> g n -> g n
  removeEdge :: Edge n -> g n -> g n
  removeNode :: n -> g n -> g n
  addNodes :: [n] -> g n -> g n
  addEdge :: LEdge l n -> g n -> g n
  hasNode :: n -> g n -> Bool
  updateNode :: (n -> n) -> n -> g n -> g n
  transpose :: g n -> g n
  bfs :: [n] -> g n -> [[n]]
  subgraph :: (n -> Bool) -> g n -> g n
  -- | Result includes starting search node
  reachable :: n -> g n -> [n]
  -- | A default dummy node that can be used

class Identifiable n i where
  getNodeId :: n -> NodeId i

-- | Simple definition for using a node as own identifier.
instance Identifiable a a where
  getNodeId = NodeId . identity

newtype Dominators a = Dominators (HashMap a (HashSet a))
  deriving (Eq, Ord, Show, Generic)

newtype PostDominators a = PostDominators (HashMap a (HashSet a))
  deriving (Eq, Ord, Show, Generic)

class DominatorMapping m where
  domEmpty :: m a
  domMap :: Hashable b => (a -> b) -> m a -> m b
  domMapMaybe :: Hashable b => (a -> Maybe b) -> m a -> m b
  domLookup :: Hashable a => a -> m a -> Maybe (HashSet a)
  domMerge :: Hashable a => m a -> m a -> m a
  domRemoveNode :: Hashable a => a -> m a -> m a
  domRemoveNode x = domMapMaybe f where
    f y = if x == y then Nothing else Just y

instance DominatorMapping Dominators where
  domEmpty = Dominators HMap.empty
  domMap f (Dominators m) = Dominators $ mapDominatorsHelper f m
  domMapMaybe f (Dominators m) = Dominators $ mapMaybeDominatorsHelper f m
  domLookup x (Dominators m) = HMap.lookup x m
  domMerge (Dominators a) (Dominators b) = Dominators $ domMergeHelper a b

instance DominatorMapping PostDominators where
  domEmpty = PostDominators HMap.empty
  domMap f (PostDominators m) = PostDominators $ mapDominatorsHelper f m
  domMapMaybe f (PostDominators m) = PostDominators $ mapMaybeDominatorsHelper f m
  domLookup x (PostDominators m) = HMap.lookup x m
  domMerge (PostDominators a) (PostDominators b) = PostDominators $ domMergeHelper a b

domLookup_ :: (DominatorMapping m, Hashable a)
  => a -> m a -> HashSet a
domLookup_ k = fromMaybe HSet.empty . domLookup k


mapMaybeDominatorsHelper
  :: forall a b. Hashable b
  => (a -> Maybe b)
  -> HashMap a (HashSet a)
  -> HashMap b (HashSet b)
mapMaybeDominatorsHelper f = HMap.fromList . mapMaybe g . HMap.toList
  where
    g :: (a, HashSet a) -> Maybe (b, HashSet b)
    g (k, s) = (,) <$> f k <*> mappedSet s

    mappedSet :: HashSet a -> Maybe (HashSet b)
    mappedSet s = case mapMaybe f (HSet.toList s) of
      [] -> Nothing
      xs -> Just $ HSet.fromList xs

mapDominatorsHelper
  :: Hashable b
  => (a -> b)
  -> HashMap a (HashSet a)
  -> HashMap b (HashSet b)
mapDominatorsHelper f = HMap.map (HSet.map f) . HMap.mapKeys f

domMergeHelper
  :: Hashable a
  => HashMap a (HashSet a)
  -> HashMap a (HashSet a)
  -> HashMap a (HashSet a)
domMergeHelper = HMap.unionWith HSet.union

type DltMap a = IntMap a

type CfMap a = HashMap a Int

findNonRepeatPaths' :: (Graph l n g, Hashable n) => HashSet n -> n -> g n -> [[n]]
findNonRepeatPaths' seen start' g = case (start' :) <$> succsPaths of
  [] -> [[start']]
  xs -> xs
  where
    succs' = HSet.toList $ succs start' g `HSet.difference` seen

    succsPaths = concatMap (\s -> findNonRepeatPaths' (HSet.insert s seen) s g) succs'

findNonRepeatPaths :: (Graph l n g, Hashable n) => n -> g n -> [[n]]
findNonRepeatPaths start' = findNonRepeatPaths' (HSet.singleton start') start'

-- | finds all paths up until a repeat or a node with no succs
findAllNonRepeatPaths :: (Graph l n g, Hashable n) => g n -> [[n]]
findAllNonRepeatPaths g
  | length (nodes g) == 1 = [HSet.toList $ nodes g]
  | otherwise = do
      src <- HSet.toList $ sources g
      findNonRepeatPaths src g

findSimplePaths' :: (Graph l n g, Hashable n) => HashSet n -> n -> n -> g n -> [[n]]
findSimplePaths' seen start' end' g = fmap (start':) $ do
  succ' <- HSet.toList $ succs start' g `HSet.difference` seen
  if succ' == end'
    then return [succ']
    else findSimplePaths' (HSet.insert succ' seen) succ' end' g

findSimplePaths :: (Graph l n g, Hashable n)
                => n
                -> n
                -> g n
                -> [[n]]
findSimplePaths = findSimplePaths' HSet.empty

findAllSimplePaths :: (Graph l n g, Hashable n) => g n -> [[n]]
findAllSimplePaths g
  | length (nodes g) == 1 = [HSet.toList $ nodes g]
  | otherwise = do
      src <- HSet.toList $ sources g
      sink <- HSet.toList $ sinks g
      findSimplePaths src sink g

sources :: (Graph l n g, Hashable n) => g n -> HashSet n
sources g = HSet.filter ((== 0) . HSet.size . flip preds g) . nodes $ g

sinks :: (Graph l n g, Hashable n) => g n -> HashSet n
sinks g = HSet.filter ((== 0) . HSet.size . flip succs g) . nodes $ g

removeEdges :: (Graph l n g) => [Edge n] -> g n -> g n
removeEdges = flip $ foldr removeEdge

addEdges :: (Graph l n g) => [LEdge l n] -> g n -> g n
addEdges = flip $ foldr addEdge

reverseSpan :: (Graph l n g, Hashable n) => g n -> Int -> n -> [[n]]
reverseSpan _ 0 node = [[node]]
reverseSpan g depth node = case HSet.toList $ preds node g of
  [] -> [[node]]
  xs -> fmap (node:) . concatMap (reverseSpan g (depth - 1)) $ xs

findAllSimplePaths2 :: forall l n g. (Graph l n g, Hashable n)
                    => g n -> n -> [[n]]
findAllSimplePaths2 g startNode =
  let m = mkNonLoopingNodeMap m (HSet.toList $ nodes g) in
    m ! startNode
  where
    mkNonLoopingNodeMap :: HashMap n [[n]] -> [n] -> HashMap n [[n]]
    mkNonLoopingNodeMap m ns = HMap.fromList $ do
      n <- ns
      let succPaths = concatMap (\s -> case m ! s of
                                        [] -> [[s]]
                                        xs -> (s:) <$> xs)
                      . HSet.toList $ succs n g
      return (n, succPaths)

countAllSimplePaths :: forall l n g. (Graph l n g, Hashable n)
                    => g n -> HashMap n Integer
countAllSimplePaths g =
  let m = mkNonLoopingNodeMap m (HSet.toList $ nodes g) in
    m
  where
    mkNonLoopingNodeMap :: HashMap n Integer -> [n] -> HashMap n Integer
    mkNonLoopingNodeMap m ns = HMap.fromList $ do
      n <- ns
      let ss = HSet.toList $ succs n g
      let x = case ss of
            [] -> 1
            xs -> sum . fmap (m !) $ xs
      return (n, x)

maxSimplePaths :: forall l n g. (Graph l n g, Hashable n)
               => g n -> Integer
maxSimplePaths = foldr max 0 . countAllSimplePaths

-- -- The total number of
-- descendantFrequencyCount :: forall e node g. (Graph e node g, Hashable n)
--                     => g -> HashMap node (HashMap node Int)
-- descendantFrequencyCount g =
--   let m = mkNonLoopingNodeMap m (HSet.toList $ nodes g) in
--     m
--   where
--     mkNonLoopingNodeMap :: HashMap node Integer -> [node] -> HashMap node Integer
--     mkNonLoopingNodeMap m ns = HMap.fromList $ do
--       n <- ns
--       let ss = HSet.toList $ succs n g
--       let x = case ss of
--             [] -> 1
--             xs -> foldr (+) 0 . fmap (m !) $ xs
--       return (n, x)

newtype DescendantsMap node = DescendantsMap (HashMap node (HashSet node))
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

-- | Slowly calculate descendants for each node. O(m * log n * n)
calcDescendantsMap :: forall l n g. (Graph l n g, Hashable n)
            => g n -> DescendantsMap n
calcDescendantsMap g = DescendantsMap
  . HMap.fromList
  . fmap (toSnd $ HSet.fromList . flip reachable g)
  . HSet.toList
  $ nodes g

-- | Quicly calculate descendants for each node. Requires that g has no loops.
calcDescendantsMapForAcyclicGraph :: forall l n g. (Graph l n g, Hashable n)
            => g n -> DescendantsMap n
calcDescendantsMapForAcyclicGraph g =
  let m = mkNonLoopingNodeMap m (HSet.toList $ nodes g) in
    DescendantsMap m
  where
    mkNonLoopingNodeMap :: HashMap n (HashSet n) -> [n] -> HashMap n (HashSet n)
    mkNonLoopingNodeMap m ns = HMap.fromList $ do
      n <- ns
      let ss = HSet.toList $ succs n g
      let x = case ss of
            [] -> HSet.empty
            xs -> foldr HSet.union HSet.empty . fmap (\s -> HSet.insert s $ m ! s) $ xs
      return (n, x)

newtype DescendantsDistanceMap node
  = DescendantsDistanceMap (HashMap node (HashMap node Int))
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

-- | Slowly calculate descendants for each node. O(m * log n * n)
calcDescendantsDistanceMap
  :: forall l n g. (Graph l n g, Hashable n)
  => g n -> DescendantsDistanceMap n
calcDescendantsDistanceMap g = DescendantsDistanceMap
  . HMap.fromList
  . fmap (toSnd $ flip calcDescendantsDistances g)
  . HSet.toList
  $ nodes g

calcDescendantsDistances
  :: forall l n g. (Graph l n g, Hashable n)
  => n -> g n -> HashMap n Int
calcDescendantsDistances n = HMap.fromList .concatMap f . zip [1..] . bfs [n]
  where
    f :: (Int, [node]) -> [(node, Int)]
    f (d, ns) = fmap (,d) ns

getDescendantDistance
  :: (Hashable n)
  => DescendantsDistanceMap n -> n -> n -> Maybe Int
getDescendantDistance (DescendantsDistanceMap m) srcNode dstNode =
  HMap.lookup srcNode m >>= HMap.lookup dstNode

-- assumes DescendantMap contains start node and is derived from g...
searchBetween_ :: forall l n g. (Graph l n g, Hashable n)
              => g n -> DescendantsMap n -> n -> n -> [[n]]
searchBetween_ g (DescendantsMap dm) start end
  | start == end = return [end]
  | HSet.member end (dm ! start) = do
      kid <- HSet.toList $ succs start g
      kidPath <- searchBetween_ g (DescendantsMap dm) kid end
      return $ start : kidPath
  | otherwise = []

{- HLINT ignore searchBetween "Eta reduce" -}
searchBetween :: forall l n g. (Graph l n g, Hashable n)
              => g n -> n -> n -> [[n]]
searchBetween g start end = searchBetween_ g (calcDescendantsMap g) start end


-- -- finding parents is (n * log(n)) for each node
-- -- this
-- parentMap :: forall e node g. (Graph e node g, Hashable n)
--           => g -> HashMap n (HashSet n)

siblings :: forall l n g. (Graph l n g, Hashable n)
         => n -> n -> g n -> HashSet n
siblings child parent g = HSet.delete child $ succs parent g

-- | Creates a graph in which every edge from a -> b also goes for b -> a.
--   For every (a, b), edge (b, a) is created using (a, b)'s edge label
--   If (b, a) already exists, it retains its label.
mkBiDirectional :: (Graph l n g, Hashable n) => g n -> g n
mkBiDirectional g = foldr f g edges'
  where
    edges' = edges g
    edgeSet = HSet.fromList . fmap (view #edge) $ edges'
    f (LEdge lbl (Edge a b)) g'
      | HSet.member (Edge b a) edgeSet = g'
      | otherwise = addEdge (LEdge lbl (Edge b a)) g'

getWeaklyConnectedComponents
  :: (Graph l n g, Hashable n)
  => g n
  -> [HashSet n]
getWeaklyConnectedComponents g = snd
  . foldr f (HSet.empty, [])
  . HSet.toList
  . nodes
  $ bi
  where
    bi = mkBiDirectional g
    f n (seen, comps)
      | HSet.member n seen = (seen, comps)
      | otherwise = let s = HSet.fromList (reachable n bi) in
          (HSet.union seen s, s:comps)

-- | Returns all descendants of a node, excluding itself,
-- unless a loop makes it its own descendent.
getDescendants :: (Graph l n g, Hashable n) => n -> g n -> HashSet n
getDescendants v g = HSet.fromList
  . concat
  . flip bfs g
  . HSet.toList
  . succs v
  $ g

-- | Returns all ancestors of a node, excluding itself,
-- unless a loop makes it its own ancestor.
getAncestors :: (Graph l n g, Hashable n) => n -> g n -> HashSet n
getAncestors v = getDescendants v . transpose

-- | Gets nodes that match predicate
getMatchingNodes :: (Graph l n g, Hashable n) => (n -> Bool) -> g n -> HashSet n
getMatchingNodes f g = HSet.filter f $ nodes g

-- | Returns all nodes with zero succs or that are self-looping
getTermNodes :: (Graph l n g, Hashable n) => g n -> HashSet n
getTermNodes g = getMatchingNodes f g
  where
    f n = case HSet.toList $ succs n g of
            [] -> True
            [x] -> n == x
            _ -> False

getFullEdge :: (Graph l n g) => Edge n -> g n -> Maybe (LEdge l n)
getFullEdge e g = (`LEdge` e) <$> getEdgeLabel e g

predEdges_ :: (Hashable n, Graph l n g) => n -> g n -> HashSet (Edge n)
predEdges_ n = HSet.map (`Edge` n) . preds n

succEdges_ :: (Hashable n, Graph l n g) => n -> g n -> HashSet (Edge n)
succEdges_ n = HSet.map (Edge n) . succs n

predEdges :: (Hashable l, Hashable n, Graph l n g) => n -> g n -> HashSet (LEdge l n)
predEdges n g
  = HSet.fromList 
  . mapMaybe (`getFullEdge` g) 
  . HSet.toList 
  $ predEdges_ n g

succEdges :: (Hashable l, Hashable n, Graph l n g) => n -> g n -> HashSet (LEdge l n)
succEdges n g
  = HSet.fromList
  . mapMaybe (\succ' -> getFullEdge (Edge n succ') g)
  . HSet.toList
  $ succs n g
