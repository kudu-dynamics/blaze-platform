{- HLINT ignore "Eta reduce" -}

module Blaze.Path
  ( module Blaze.Path
  , module Exports
  )
where

import Blaze.Prelude
import Blaze.Types.Graph (Graph, DescendantsMap, LEdge(LEdge), Edge(Edge))
import qualified Blaze.Types.Graph as G
import Blaze.Types.Path as Exports
import qualified Blaze.Types.Path as P

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet


-- | Counts the number of times each node has been visited.
newtype VisitCounts n = VisitCounts { unVisitCount :: HashMap n Int }
  deriving (Eq, Ord, Show, Generic)

updateVisitCounts :: Hashable n => n -> VisitCounts n -> VisitCounts n
updateVisitCounts n = VisitCounts . HashMap.alter (Just . maybe 1 (+1)) n . unVisitCount

getVisitCount :: Hashable n => n -> VisitCounts n -> Int
getVisitCount n = fromMaybe 0 . HashMap.lookup n . unVisitCount

visitCountsFromList :: Hashable n => [(n, Int)] -> VisitCounts n
visitCountsFromList = VisitCounts . HashMap.fromList

-- | Gets paths that might loop. Must specifiy revisit limit to ensure this terminates
-- The revisit limit specifies how many times a specific node may be revisited in a
-- single path (maybe useful for loop unrolling).
-- The `changeOnRevisit` function should change the node using the revisit count to
-- ensure that the revisited node is unique from the original.
-- TODO: we could make this run in a monad and the changeOnRevisit function could run in
-- the monad and keep track of state.
getAllPaths
  :: forall l n g p. (Graph l n g, IsPath l n p, Hashable n, Hashable l)
  => (Int -> n -> n)
  -> Int
  -> n
  -> g n
  -> [p n]
getAllPaths changeOnRevisit revisitLimit startNode g
  | not (G.hasNode startNode g) = [] -- start node not in graph
  | otherwise = P.build
    <$> traverseAllPaths (visitCountsFromList [(startNode, 1)]) (P.start startNode) (getSuccEdges startNode)
  where
    getSuccEdges n = HashSet.toList $ G.succEdges n g

    traverseAllPaths :: VisitCounts n -> PathBuilder l n -> [LEdge l n] -> [PathBuilder l n]
    traverseAllPaths visitCounts pb es =
      case concatMap followSucc es of
        [] -> [pb]
        pbs -> pbs
      where
        followSucc :: LEdge l n -> [PathBuilder l n]
        followSucc (LEdge l (Edge _ b)) =
          if visitCount > revisitLimit + 1
          then []
          else traverseAllPaths visitCounts' (pb -| l |- b') (getSuccEdges b)
          where
            visitCounts' = updateVisitCounts b visitCounts
            visitCount = getVisitCount b visitCounts'
            b' = bool b (changeOnRevisit visitCount b) $ visitCount > 1

-- | Simple paths go from the root to a term node and don't revisit the same node twice
getAllSimplePaths :: (Hashable n, Hashable l, Graph l n g, IsPath l n p) => n -> g n -> [p n]
getAllSimplePaths = getAllPaths (\_ _ -> error "Should not revisit") 0

-- | Returns only paths that contain all the required nodes
-- TODO: is it worth the added comlexity of using the DescendantsMap to find only paths
-- that contain the required nodes? It still mostly won't save us from path explosion in
-- extremely large functions. Maybe it would be better to `getAllPaths` and filter the
-- results.
getPathsContaining_
  :: forall l n g p. (Graph l n g, IsPath l n p, Hashable n, Hashable l)
  => DescendantsMap n
  -> (Int -> n -> n)
  -> Int
  -> n
  -> g n
  -> HashSet n
  -> [p n]
getPathsContaining_ (G.DescendantsMap dmap) changeOnRevisit revisitLimit startNode g requiredNodes
  | not (G.hasNode startNode g) = [] -- start node not in graph
  | not (HashSet.isSubsetOf requiredNodes $ getDescendants startNode) = [] -- cannot fulfill req nodes
  | otherwise = P.build
    <$> traverseAllPaths (HashSet.delete startNode requiredNodes) (visitCountsFromList [(startNode, 1)]) (P.start startNode) (getSuccEdges startNode)
  where
    getSuccEdges n = HashSet.toList $ G.succEdges n g

    getDescendants :: n -> HashSet n
    getDescendants n = fromJust $ HashMap.lookup n dmap
  
    traverseAllPaths :: HashSet n -> VisitCounts n -> PathBuilder l n -> [LEdge l n] -> [PathBuilder l n]
    traverseAllPaths reqNodes visitCounts pb es =
      case concatMap followSucc $ filter onlySuccsThatLeadToAllReqs es of
        [] -> if HashSet.null reqNodes then [pb] else []
        pbs -> pbs
      where
        onlySuccsThatLeadToAllReqs :: LEdge l n -> Bool
        onlySuccsThatLeadToAllReqs (LEdge _ (Edge _ b)) =
          HashSet.isSubsetOf reqNodes $ getDescendants b

        followSucc :: LEdge l n -> [PathBuilder l n]
        followSucc (LEdge l (Edge _ b)) =
          if visitCount > revisitLimit + 1
          then []
          else traverseAllPaths reqNodes' visitCounts' (pb -| l |- b') (getSuccEdges b)
          where
            reqNodes' = HashSet.delete b reqNodes
            visitCounts' = updateVisitCounts b visitCounts
            visitCount = getVisitCount b visitCounts'
            b' = bool b (changeOnRevisit visitCount b) $ visitCount > 1

-- | Returns only paths that contain all the required nodes
getPathsContaining
  :: (Graph l n g, IsPath l n p, Hashable n, Hashable l)
  => (Int -> n -> n)
  -> Int
  -> n
  -> g n
  -> HashSet n
  -> [p n]
getPathsContaining changeOnRevisit revisitLimit startNode g
  = getPathsContaining_ (G.calcDescendantsMap g) changeOnRevisit revisitLimit startNode g

-- | Returns only paths that contain all the required nodes
--   but don't contain any of the avoidNodes
-- Note: this will be really tricky. You have to make sure the graph can lead to all the
-- remaining required nodes, but make sure you can get to the required nodes without
-- having to go through an avoid node.
getPathsContainingAndAvoiding_
  :: (Graph l n g, IsPath l n p, Hashable n, Hashable l)
  => DescendantsMap n
  -> (Int -> n -> n)
  -> Int  
  -> n
  -> g n
  -> HashSet n
  -> HashSet n
  -> [p n]
getPathsContainingAndAvoiding_ dmap changeOnRevisit revisitLimit startNode g requiredNodes avoidNodes = filter (HashSet.null . HashSet.intersection avoidNodes . P.nodes) pathsContaining
  where
    pathsContaining = getPathsContaining_ dmap changeOnRevisit revisitLimit startNode g requiredNodes


-- | Returns only paths that contain all the required nodes
--   but don't contain any of the avoidNodes
getPathsContainingAndAvoiding
  :: (Graph l n g, IsPath l n p, Hashable n, Hashable l)
  => (Int -> n -> n)
  -> Int
  -> n
  -> g n
  -> HashSet n
  -> HashSet n
  -> [p n]
getPathsContainingAndAvoiding changeOnRevisit revisitLimit startNode g requiredNodes avoidNodes
  = getPathsContainingAndAvoiding_ (G.calcDescendantsMap g) changeOnRevisit revisitLimit startNode g requiredNodes avoidNodes
