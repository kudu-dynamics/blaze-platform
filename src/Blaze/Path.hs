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
import qualified Data.List.NonEmpty as NE

import System.Random (randomRIO)

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
  :: forall l n g p. (Graph l n g, PathConstruct l n p, Hashable n, Hashable l)
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
getAllSimplePaths :: (Hashable n, Hashable l, Graph l n g, PathConstruct l n p) => n -> g n -> [p n]
getAllSimplePaths = getAllPaths (\_ _ -> error "Should not revisit") 0

-- | Returns only paths that contain all the required nodes
-- TODO: is it worth the added comlexity of using the DescendantsMap to find only paths
-- that contain the required nodes? It still mostly won't save us from path explosion in
-- extremely large functions. Maybe it would be better to `getAllPaths` and filter the
-- results.
getPathsContaining_
  :: forall l n g p. (Graph l n g, PathConstruct l n p, Hashable n, Hashable l)
  => DescendantsMap n
  -> (Int -> n -> n)
  -> Int
  -> n
  -> g n
  -> HashSet n
  -> [p n]
getPathsContaining_ dmap changeOnRevisit revisitLimit startNode g requiredNodes
  | not (G.hasNode startNode g) = [] -- start node not in graph
  | not (HashSet.isSubsetOf requiredNodes $ unsafeGetDescendants dmap startNode) = [] -- cannot fulfill req nodes
  | otherwise = P.build
    <$> traverseAllPaths (HashSet.delete startNode requiredNodes) (visitCountsFromList [(startNode, 1)]) (P.start startNode) (getSuccEdges startNode)
  where
    getSuccEdges n = HashSet.toList $ G.succEdges n g
  
    traverseAllPaths :: HashSet n -> VisitCounts n -> PathBuilder l n -> [LEdge l n] -> [PathBuilder l n]
    traverseAllPaths reqNodes visitCounts pb es =
      case concatMap followSucc $ filter onlySuccsThatLeadToAllReqs es of
        [] -> if HashSet.null reqNodes then [pb] else []
        pbs -> pbs
      where
        onlySuccsThatLeadToAllReqs :: LEdge l n -> Bool
        onlySuccsThatLeadToAllReqs (LEdge _ (Edge _ b)) =
          HashSet.isSubsetOf reqNodes $ unsafeGetDescendants dmap b

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
  :: (Graph l n g, PathConstruct l n p, Hashable n, Hashable l)
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
  :: (Graph l n g, IsPath l n p, PathConstruct l n p, Hashable n, Hashable l)
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
  :: (Graph l n g, IsPath l n p, PathConstruct l n p, Hashable n, Hashable l)
  => (Int -> n -> n)
  -> Int
  -> n
  -> g n
  -> HashSet n
  -> HashSet n
  -> [p n]
getPathsContainingAndAvoiding changeOnRevisit revisitLimit startNode g requiredNodes avoidNodes
  = getPathsContainingAndAvoiding_ (G.calcDescendantsMap g) changeOnRevisit revisitLimit startNode g requiredNodes avoidNodes

--- Sampling

data SampleRandomPathError e
  = StartNodeNotInGraph
  | BranchChooserError e
  deriving (Eq, Ord, Show, Generic)

-- | Gets all nodes reachable by node n.
--   Throws error if node is not in map.
unsafeGetDescendants :: Hashable n => DescendantsMap n -> n -> HashSet n
unsafeGetDescendants (G.DescendantsMap dmap) n = case HashMap.lookup n dmap of
  Nothing -> error "Could not find node in descendants map"
  Just s -> s

-- | Gets all nodes reachable by node n.
--   Throws error if node is not in map.
getDescendants :: Hashable n => DescendantsMap n -> n -> Maybe (HashSet n)
getDescendants (G.DescendantsMap dmap) n = HashMap.lookup n dmap

-- | Returns True of needles is empty or if haystack has at least one needle
hasAtLeastOneOf :: Hashable n => HashSet n -> HashSet n -> Bool
hasAtLeastOneOf haystack needles =
  HashSet.null needles
  || (not . HashSet.null $ needles `HashSet.intersection` haystack)

-- | If one req node is found in the reqSomeNodes set, then we don't
-- want to steer the randomizer to find the other ones, so we clear them.
-- TODO: instead of having a HashSet.empty represent the "no-req-nodes" state,
-- use a `Maybe`
clearAllNodesIfFound :: Hashable n => n -> HashSet n -> HashSet n
clearAllNodesIfFound n s = if HashSet.member n s then HashSet.empty else s

-- | Gets succs that have not been visited too many times
getValidSuccs
  :: forall l n g. (Graph l n g, Hashable n, Hashable l)
  => Int
  -> g n
  -> VisitCounts n
  -> n
  -> [(l, n)]
getValidSuccs revisitLimit g visitCounts n =
  mapMaybe getValidSucc . HashSet.toList $ G.succEdges n g
  where
    getValidSucc (G.LEdge lbl (G.Edge _ b)) =
      if getVisitCount b visitCounts > revisitLimit
      then Nothing
      else Just (lbl, b)

-- | A function that chooses between multiple children when taking a path sample.
-- If the result is Nothing, the path will be created, but will end at that parent node.
--- state -> parentNode -> childrenHalfEdges -> (state', Maybe child)
type ChildChooser s e m l n = s -> n -> NonEmpty (l, n) -> ExceptT e m (Maybe (s, (l, n)))

sampleRandomPath_'
  :: forall s e l n g p m. (Monad m, Graph l n g, PathConstruct l n p, Hashable n, Hashable l)
  => ChildChooser s e m l n -- Choose between children
  -> s -- initial chooser state
  -> (Int -> n -> n)
  -> Int
  -> n
  -> g n
  -> ExceptT (SampleRandomPathError e) m (p n)
sampleRandomPath_' chooseChild initialChooserState _changeOnRevisit revisitLimit startNode g
  | not (G.hasNode startNode g) = throwError StartNodeNotInGraph
  | otherwise = do
      p <- withExceptT BranchChooserError
        $ getRestOfPath initialChooserState (visitCountsFromList []) startNode (P.start startNode)
      return $ P.build p
  where
    getRestOfPath :: s -> VisitCounts n -> n -> PathBuilder l n -> ExceptT e m (PathBuilder l n)
    getRestOfPath chooserState visitCounts prevNode prevPath = do
      let visitCounts' = updateVisitCounts prevNode visitCounts
      case getValidSuccs revisitLimit g visitCounts' prevNode of
        [] -> return prevPath
        (x:xs) -> chooseChild chooserState prevNode (x :| xs) >>= \case
          Nothing -> return prevPath
          Just (s, (lbl, n)) -> getRestOfPath s visitCounts' n $ prevPath -| lbl |- n

-- | Picks out of list based on int in fst of tuple.
-- The larger the int means greater chance of getting picked.
-- Errors if n's total to zero.  
stochasticChoice
  :: Monad m
  => ((Int, Int) -> m Int)
  -> NonEmpty (Int, x)
  -> m x
stochasticChoice _ ((_, x) :| []) = return x
stochasticChoice pickFromRange (y :| ys) =
  if total == 0 then
    error "Total sum should not equal zero"
  else do
    pick <- pickFromRange (0, total - 1)
    return $ roulette (y :| ys) pick
  where
    total = sum . fmap (view _1) $ y : ys

-- | Imagine a roulette wheel where the number of slots is equal to the sum total
-- of all the `a` numbers in the list. Each `x` is covering `a` number of slots.
-- `pick` is a number between 0 and the sum total of the `a`s.
-- This function gets the `x` that `pick` lands on.
roulette :: (Ord a, Num a) => NonEmpty (a, x) -> a -> x
roulette ((_, x) :| [])  _ = x
roulette ((n, x) :| (y:ys)) pick
  | pick < n = x
  | otherwise = roulette (y :| ys) $ pick - n


data ChooseChildError n
  = StartNodeNotFoundInDescendantMap n
  | ChildNodeNotFoundInDescendantMap n
  | NoReqNodesCanBeReached
  deriving (Eq, Ord, Show, Generic)

-- | This chooses a child randomly, based on descendant count.
chooseChildByDescendantCount
  :: forall m l n. (Monad m, Hashable n)
  => ((Int, Int) -> m Int)
  -> DescendantsMap n
  -> ChildChooser () (ChooseChildError n) m l n
chooseChildByDescendantCount pickFromRange dmap _ _parentNode childrenHalfEdges = do
  ratedChildren <- mapM rateChild childrenHalfEdges
  choice <- lift $ stochasticChoice pickFromRange ratedChildren
  return $ Just ((), choice)
  where
    rateChild :: (l, n) -> ExceptT (ChooseChildError n) m (Int, (l, n))
    rateChild (l, n) = case getDescendants dmap n of
      Nothing -> throwError $ ChildNodeNotFoundInDescendantMap n
      Just s -> return (fromIntegral $ HashSet.size s, (l, n))

data ReqNodesState n
  = InitReqNodes (HashSet n)
  | FindingReqNode (HashSet n)
  | FoundReqNode
  deriving (Eq, Ord, Show, Generic)

-- | This chooses a child randomly, based on descendant count.
-- It only takes paths that reach at least one of the req nodes.
-- If no req nodes can be reached at the start, it returns Nothing.
chooseChildByDescendantCountAndReqSomeNodes
  :: forall m l n. (Monad m, Hashable n)
  => ((Int, Int) -> m Int)
  -> DescendantsMap n
  -> ChildChooser (ReqNodesState n) (ChooseChildError n) m l n
chooseChildByDescendantCountAndReqSomeNodes pickFromRange dmap reqSomeNodesState parentNode childrenHalfEdges = case reqSomeNodesState of
  InitReqNodes reqSomeNodes -> case getDescendants dmap parentNode of
    Nothing -> throwError $ StartNodeNotFoundInDescendantMap parentNode
    Just descendants -> do
      let reqNodesThatAreDescendants = HashSet.intersection descendants reqSomeNodes
      if HashSet.null reqNodesThatAreDescendants
        then throwError NoReqNodesCanBeReached
        else chooseChildByDescendantCountAndReqSomeNodes pickFromRange dmap (FindingReqNode reqNodesThatAreDescendants) parentNode childrenHalfEdges
  FindingReqNode reqSomeNodes ->
    if HashSet.member parentNode reqSomeNodes then
    -- If parent is found in reqSomeNodes, clear other req nodes, since at least one has been reached.
      chooseChildByDescendantCountAndReqSomeNodes pickFromRange dmap FoundReqNode parentNode childrenHalfEdges
    else do
      childrenThatReachSomeNodes <- flip filterM (NE.toList childrenHalfEdges) $ \(_, n) -> do
        descs <- liftMaybe (ChildNodeNotFoundInDescendantMap n) $ getDescendants dmap n
        return . not . HashSet.null $ HashSet.intersection reqSomeNodes descs
      case childrenThatReachSomeNodes of
        [] -> throwError NoReqNodesCanBeReached
        (x:xs) -> useDescendantCountChildChooser (FindingReqNode reqSomeNodes) $ x :| xs
  FoundReqNode -> useDescendantCountChildChooser FoundReqNode childrenHalfEdges
  where
    useDescendantCountChildChooser st children = do
      chooseChildByDescendantCount pickFromRange dmap () parentNode children >>= \case
        Nothing -> return Nothing
        Just (_, x) -> return $ Just (st, x)

-- -- | Gets a random path.
-- -- Returns only paths that at least one of the required nodes.
-- -- Random choice is weighted according to number of descendants,
-- -- so if one branch has 10 descendent nodes and the second has 5,
-- -- the first node has 2x chance of being selected.
-- -- `revisitLmit` is the number of times a node can be visited in a single path,
-- -- (i.e. because of loops)
-- --
-- -- TODO: pass in function to choose between multiple child branches, i.e:
-- --  (parent node -> [(edgelbl, child node)] -> m (child node))
-- sampleRandomPath_
--   :: forall l n g p m. (Monad m, Graph l n g, PathConstruct l n p, Hashable n, Hashable l)
--   => ((Int, Int) -> m Int)
--   -> DescendantsMap n
--   -> (Int -> n -> n)
--   -> Int
--   -> n
--   -> g n
--   -> HashSet n
--   -> m (Either SampleRandomPathError (p n))
-- sampleRandomPath_ getRandomInRange dmap _changeOnRevisit revisitLimit startNode g reqSomeNodes
--   | not (G.hasNode startNode g) = return $ Left StartNodeNotInGraph
--   | not (unsafeGetDescendants dmap startNode `hasAtLeastOneOf` reqSomeNodes) = return $ Left CannotReachRequiredNodes -- cannot fulfill req nodes
--   | otherwise = do
--       p <- getRestOfPath (clearAllNodesIfFound startNode reqSomeNodes) (visitCountsFromList []) startNode (P.start startNode)
--       return . Right $ P.build p
--   where
--     -- | This descends to succs from a starting node, building up the path as it goes.
--     -- If there are multiple succs, it chooses between them using stochastic selection,
--     -- where those with more reachable descendant nodes are more likely to be selected.
--     -- visitCounts is used to filter out all the succs that have already been used in the path
--     -- (to prevent following loops).
--     getRestOfPath :: HashSet n -> VisitCounts n -> n -> PathBuilder l n -> m (PathBuilder l n)
--     getRestOfPath reqNodes visitCounts prevNode prevPath = do
--       let visitCounts' = updateVisitCounts prevNode visitCounts
--       case getValidSuccs (unsafeGetDescendants dmap) revisitLimit g visitCounts' reqNodes prevNode of
--         [] -> return prevPath
--         [(lbl, n, _)] -> getRestOfPath (clearAllNodesIfFound n reqNodes) visitCounts' n $ prevPath -| lbl |- n
--         xs -> do
--           let totalDesc = sum . fmap (view _3) $ xs
--           pick <- getRandomInRange (0, totalDesc - 1)
--           let (lbl, n) = roulette xs pick
--           getRestOfPath (HashSet.delete n reqNodes) visitCounts' n $ prevPath -| lbl |- n
--             where
--               roulette [] _ = error "Pick too high for roulette wheel"
--               roulette ((lbl, n, descCount):zs) pick
--                 | pick < descCount = (lbl, n)
--                 | otherwise = roulette zs $ pick - descCount

type SampleRandomPathError' n = SampleRandomPathError (ChooseChildError n)

sampleRandomPath
  :: forall l n g p. (Graph l n g, PathConstruct l n p, Hashable n, Hashable l)
  => DescendantsMap n
  -> (Int -> n -> n)
  -> Int
  -> n
  -> g n
  -> HashSet n
  -> IO (Either (SampleRandomPathError (ChooseChildError n)) (p n))
sampleRandomPath = sampleRandomPath_ randomRIO

sampleRandomPath_
  :: forall l n g p m. (Monad m, Graph l n g, PathConstruct l n p, Hashable n, Hashable l)
  => ((Int, Int) -> m Int)
  -> DescendantsMap n
  -> (Int -> n -> n)
  -> Int
  -> n
  -> g n
  -> HashSet n
  -> m (Either (SampleRandomPathError (ChooseChildError n)) (p n))
sampleRandomPath_ getRandomInRange dmap _changeOnRevisit revisitLimit startNode g reqSomeNodes =
  runExceptT $
  sampleRandomPath_'
  (chooseChildByDescendantCountAndReqSomeNodes getRandomInRange dmap)
  (if HashSet.null reqSomeNodes then FoundReqNode else InitReqNodes reqSomeNodes)
  _changeOnRevisit
  revisitLimit
  startNode
  g
