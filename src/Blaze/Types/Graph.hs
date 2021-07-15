module Blaze.Types.Graph where

import Blaze.Prelude hiding (transpose)

import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

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

-- TODO: Switch to HashSet from Set for type class
class Graph e attr n g | g -> e attr n where
  empty :: g
  fromNode :: n -> g
  fromEdges :: [LEdge e n] -> g
  succs :: n -> g -> HashSet n
  preds :: n -> g -> HashSet n
  nodes :: g -> HashSet n
  edges :: g -> [LEdge e n]
  getEdgeLabel :: Edge n -> g -> Maybe e
  setEdgeLabel :: e -> Edge n -> g -> g
  getNodeAttr :: n -> g -> Maybe attr
  setNodeAttr :: attr -> n -> g -> g
  removeEdge :: Edge n -> g -> g
  removeNode :: n -> g -> g
  addNodes :: [n] -> g -> g
  addNodesWithAttrs :: [(n, attr)] -> g -> g
  addEdge :: LEdge e n -> g -> g
  hasNode :: n -> g -> Bool
  transpose :: g -> g
  bfs :: [n] -> g -> [[n]]
  subgraph :: (n -> Bool) -> g -> g
  reachable :: n -> g -> [n]


findNonRepeatPaths' :: (Graph e attr n g, Hashable n, Eq n) => HashSet n -> n -> g -> [[n]]
findNonRepeatPaths' seen start' g = case (start' :) <$> succsPaths of
  [] -> [[start']]
  xs -> xs
  where
    succs' = HashSet.toList $ succs start' g `HashSet.difference` seen

    succsPaths = concatMap (\s -> findNonRepeatPaths' (HashSet.insert s seen) s g) succs'

findNonRepeatPaths :: (Graph e attr n g, Hashable n, Eq n) => n -> g -> [[n]]
findNonRepeatPaths start' = findNonRepeatPaths' (HashSet.singleton start') start'

-- | finds all paths up until a repeat or a node with no succs
findAllNonRepeatPaths :: (Graph e attr node g, Hashable node, Eq node) => g -> [[node]]
findAllNonRepeatPaths g 
  | length (nodes g) == 1 = [HashSet.toList $ nodes g]
  | otherwise = do
      src' <- HashSet.toList $ sources g
      findNonRepeatPaths src' g

findSimplePaths' :: (Graph e attr n g, Hashable n, Eq n) => HashSet n -> n -> n -> g -> [[n]]
findSimplePaths' seen start' end' g = fmap (start':) $ do
  succ' <- HashSet.toList $ succs start' g `HashSet.difference` seen
  if succ' == end'
    then return [succ']
    else findSimplePaths' (HashSet.insert succ' seen) succ' end' g

findSimplePaths :: (Graph e attr node g, Hashable node, Eq node)
                => node
                -> node
                -> g
                -> [[node]]
findSimplePaths = findSimplePaths' HashSet.empty

findAllSimplePaths :: (Graph e attr node g, Hashable node, Eq node) => g -> [[node]]
findAllSimplePaths g 
  | length (nodes g) == 1 = [HashSet.toList $ nodes g]
  | otherwise = do
      src' <- HashSet.toList $ sources g
      sink <- HashSet.toList $ sinks g
      findSimplePaths src' sink g

sources :: Graph e attr n g => g -> HashSet n
sources g = HashSet.filter ((== 0) . HashSet.size . flip preds g) . nodes $ g

sinks :: Graph e attr n g => g -> HashSet n
sinks g = HashSet.filter ((== 0) . HashSet.size . flip succs g) . nodes $ g

removeEdges :: Graph e attr n g => [Edge n] -> g -> g
removeEdges = flip $ foldr removeEdge

addEdges :: Graph e attr n g => [LEdge e n] -> g -> g
addEdges = flip $ foldr addEdge

reverseSpan :: Graph e attr n g => g -> Int -> n -> [[n]]
reverseSpan _ 0 node = [[node]]
reverseSpan g depth node = case HashSet.toList $ preds node g of
  [] -> [[node]]
  xs -> fmap (node:) . concatMap (reverseSpan g (depth - 1)) $ xs
  

findAllSimplePaths2 :: forall e attr node g. (Graph e attr node g, Hashable node, Eq node)
                    => g -> node -> [[node]]
findAllSimplePaths2 g startNode =
  let m = mkNonLoopingNodeMap m (HashSet.toList $ nodes g) in
    m ! startNode
  where
    mkNonLoopingNodeMap :: HashMap node [[node]] -> [node] -> HashMap node [[node]]
    mkNonLoopingNodeMap m ns = HashMap.fromList $ do
      n <- ns
      let succPaths = concatMap (\s -> case m ! s of
                                        [] -> [[s]]
                                        xs -> (s:) <$> xs)
                      . HashSet.toList $ succs n g
      return (n, succPaths)


countAllSimplePaths :: forall e attr node g. (Graph e attr node g, Hashable node, Eq node)
                    => g -> HashMap node Integer
countAllSimplePaths g =
  let m = mkNonLoopingNodeMap m (HashSet.toList $ nodes g) in
    m
  where
    mkNonLoopingNodeMap :: HashMap node Integer -> [node] -> HashMap node Integer
    mkNonLoopingNodeMap m ns = HashMap.fromList $ do
      n <- ns
      let ss = HashSet.toList $ succs n g
      let x = case ss of
            [] -> 1
            xs -> sum . fmap (m !) $ xs
      return (n, x)

maxSimplePaths :: forall e attr node g. (Graph e attr node g, Hashable node, Eq node)
               => g -> Integer
maxSimplePaths = foldr max 0 . countAllSimplePaths

-- -- The total number of 
-- descendentFrequencyCount :: forall e node g. (Graph e node g, Hashable node)
--                     => g -> HashMap node (HashMap node Int)
-- descendentFrequencyCount g =
--   let m = mkNonLoopingNodeMap m (HashSet.toList $ nodes g) in
--     m
--   where
--     mkNonLoopingNodeMap :: HashMap node Integer -> [node] -> HashMap node Integer
--     mkNonLoopingNodeMap m ns = HashMap.fromList $ do
--       n <- ns
--       let ss = HashSet.toList $ succs n g
--       let x = case ss of
--             [] -> 1
--             xs -> foldr (+) 0 . fmap (m !) $ xs
--       return (n, x)

newtype DescendentsMap node = DescendentsMap (HashMap node (HashSet node))
  deriving (Eq, Ord, Show)

calcDescendentsMap :: forall e attr node g. (Graph e attr node g, Hashable node, Eq node)
            => g -> DescendentsMap node
calcDescendentsMap g =
  let m = mkNonLoopingNodeMap m (HashSet.toList $ nodes g) in
    DescendentsMap m
  where
    mkNonLoopingNodeMap :: HashMap node (HashSet node) -> [node] -> HashMap node (HashSet node)
    mkNonLoopingNodeMap m ns = HashMap.fromList $ do
      n <- ns
      let ss = HashSet.toList $ succs n g
      let x = case ss of
            [] -> HashSet.empty
            xs -> foldr HashSet.union HashSet.empty . fmap (\s -> HashSet.insert s $ m ! s) $ xs
      return (n, x)

-- assumes DescendentMap contains start node and is derived from g...
searchBetween_ :: forall e attr node g. (Graph e attr node g, Hashable node, Eq node)
              => g -> DescendentsMap node -> node -> node -> [[node]]
searchBetween_ g (DescendentsMap dm) start end
  | start == end = return [end]
  | HashSet.member end (dm ! start) = do
      kid <- HashSet.toList $ succs start g
      kidPath <- searchBetween_ g (DescendentsMap dm) kid end
      return $ start : kidPath      
  | otherwise = []

{- HLINT ignore searchBetween "Eta reduce" -}
searchBetween :: forall e attr node g. (Graph e attr node g, Hashable node, Eq node)
              => g -> node -> node -> [[node]]
searchBetween g start end = searchBetween_ g (calcDescendentsMap g) start end


-- -- finding parents is (n * log(n)) for each node
-- -- this 
-- parentMap :: forall e node g. (Graph e node g, Hashable node)
--           => g -> HashMap n (HashSet n)
          

siblings :: forall e attr node g. (Graph e attr node g, Hashable node, Eq node)
         => node -> node -> g -> HashSet node
siblings child parent g = HashSet.delete child $ succs parent g


-- TODO: Get these working with attr nodes.
-- Currently not used anywhere.
-- mapGraph :: (Graph e n g, Graph e' n' g')
--          => (e -> e') -> (n -> n') -> g -> g'
-- mapGraph ef nf = fromEdges . fmap (\ (e, (n1, n2)) -> (ef e, (nf n1, nf n2))) . edges


-- mapEdges :: (Graph e n g, Graph e' n g')
--          => (e -> e') -> g -> g'
-- mapEdges f = mapGraph f identity


-- mapNodes :: (Graph e n g, Graph e n' g') => (n -> n') -> g -> g'
-- mapNodes = mapGraph identity


updateNodeAttr :: (Graph e attr n g) => (attr -> attr) -> n -> g -> g
updateNodeAttr f n g = case getNodeAttr n g of
  Nothing -> g
  Just attr -> setNodeAttr (f attr) n g


-- | All nodes that can reach node and that can be reached by node
connectedNodes :: (Graph e attr n g, Hashable n, Eq n) => n -> g -> HashSet n
connectedNodes n g = HashSet.fromList reachedNodes <> HashSet.fromList reachingNodes
  where
    reachedNodes = reachable n g
    reachingNodes = reachable n $ transpose g

-- | Removes all nodes/edges that don't lead to or can't be reached by node
focus :: (Graph e attr n g, Hashable n, Eq n) => n -> g -> g
focus n g = foldl' (flip removeNode) g
  . HashSet.difference (nodes g)
  . connectedNodes n
  $ g
