{- HLINT ignore "Eta reduce" -}
{- HLINT ignore "Use <$>" -}

module Blaze.Path
  ( module Blaze.Path
  , module Exports
  )
where

import Blaze.Prelude
import Blaze.Types.Graph (Graph, DescendantsMap, LEdge(LEdge), Edge(Edge), StrictDescendantsMap)
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

emptyVisitCounts :: VisitCounts n
emptyVisitCounts = VisitCounts HashMap.empty

pickFromList :: Monad m => ((Int, Int) -> m Int) -> NonEmpty a -> m a
pickFromList picker (x :| xs) = do
  n <- picker (0, length xs)
  return $ (x:xs) !! n

pickFromListFp :: Monad m => m Double -> NonEmpty a -> m a
pickFromListFp randDouble (x :| xs) = do
  let len = length xs + 1 -- `xs` is missing the first element
  r <- randDouble
  let n = min (len - 1) . floor $ r * fromIntegral len
  return $ (x:xs) !! n

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
  => StrictDescendantsMap n
  -> (Int -> n -> n)
  -> Int
  -> n
  -> g n
  -> HashSet n
  -> [p n]
getPathsContaining_ dmap changeOnRevisit revisitLimit startNode g requiredNodes
  | not (G.hasNode startNode g) = [] -- start node not in graph
  | not ( HashSet.isSubsetOf requiredNodes . HashSet.insert startNode $ unsafeGetStrictDescendants dmap startNode) = [] -- cannot fulfill req nodes
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
          HashSet.isSubsetOf reqNodes . HashSet.insert b $ unsafeGetStrictDescendants dmap b

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
  = getPathsContaining_ (G.calcStrictDescendantsMap g) changeOnRevisit revisitLimit startNode g

-- | Returns only paths that contain all the required nodes
--   but don't contain any of the avoidNodes
-- Note: this will be really tricky. You have to make sure the graph can lead to all the
-- remaining required nodes, but make sure you can get to the required nodes without
-- having to go through an avoid node.
getPathsContainingAndAvoiding_
  :: (Graph l n g, IsPath l n p, PathConstruct l n p, Hashable n, Hashable l)
  => StrictDescendantsMap n
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
  = getPathsContainingAndAvoiding_ (G.calcStrictDescendantsMap g) changeOnRevisit revisitLimit startNode g requiredNodes avoidNodes


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
unsafeGetStrictDescendants :: Hashable n => StrictDescendantsMap n -> n -> HashSet n
unsafeGetStrictDescendants (G.StrictDescendantsMap dmap) n = case HashMap.lookup n dmap of
  Nothing -> error "Could not find node in descendants map"
  Just s -> s

-- | Gets all nodes reachable by node n.
--   Throws error if node is not in map.
getDescendants :: Hashable n => DescendantsMap n -> n -> Maybe (HashSet n)
getDescendants (G.DescendantsMap dmap) n = HashMap.lookup n dmap

-- | Gets all nodes reachable by node n.
--   Throws error if node is not in map.
getStrictDescendants :: Hashable n => StrictDescendantsMap n -> n -> Maybe (HashSet n)
getStrictDescendants (G.StrictDescendantsMap dmap) n = HashMap.lookup n dmap

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

getSuccHalfEdges
  :: forall l n g. (Graph l n g, Hashable n, Hashable l)
  => g n
  -> n
  -> [(l, n)]
getSuccHalfEdges g n = fmap f . HashSet.toList $ G.succEdges n g
  where
    f e = (e ^. #label, e ^. #edge . #dst)

type MonadChooser e s m a = StateT s (ExceptT e m) a

runMonadChooser :: MonadChooser e s m a -> s -> m (Either e (a, s))
runMonadChooser action st = runExceptT . flip runStateT st $ action

runMonadChooser_ :: MonadChooser e s m a -> s -> ExceptT e m (a, s)
runMonadChooser_ = runStateT

withNestedState :: Monad m => Lens' s nestedState -> StateT nestedState m a -> StateT s m a
withNestedState l action = do
  innerState <- use l
  (x, innerState') <- lift $ runStateT action innerState
  l .= innerState'
  return x

withInnerState :: Monad m => s' -> StateT s' m a -> StateT s m (a, s')
withInnerState innerState action = lift $ runStateT action innerState

-- | A function that chooses between multiple children when taking a path sample.
-- If the result is Nothing, the path will be created, but will end at that node.
-- This returns both the unmodified choice and a possibly-transformed choice.
-- Warning: the "unmodified" edge/node choice must not be modified
-- args are: state -> parentNode -> childrenHalfEdges -> Maybe (state', choice)
type ChildChooser e s m l n = n -> [(l, n)] -> MonadChooser e s m (Maybe (ChildChoice l n))

-- | Like ChildChooser, but does not transform result. Useful for composition.
type ChildChooser_ e s m l n = n -> [(l, n)] -> MonadChooser e s m (Maybe (l, n))

toFullChooser :: Functor m => ChildChooser_ e s m l n -> ChildChooser e s m l n
toFullChooser chooser a b = fmap (fmap (\x -> ChildChoice x x)) $ chooser a b

data ChildChoice l n = ChildChoice
  { unmodifiedChoice :: (l, n)
  , modifedChoiceForPathInclusion :: (l, n) -- | This will go into the path
  } deriving (Eq, Ord, Show, Generic)

sampleRandomPath_'
  :: forall s e l n g p m. (Monad m, Graph l n g, PathConstruct l n p, Hashable n, Hashable l)
  => ChildChooser e s m l n -- Choose between children
  -> s -- initial chooser state
  -> (Int -> n -> n)
  -> Int
  -> n
  -> g n
  -> ExceptT (SampleRandomPathError e) m (p n)
sampleRandomPath_' chooseChild initialChooserState _changeOnRevisit revisitLimit startNode g
  | not (G.hasNode startNode g) = throwError StartNodeNotInGraph
  | otherwise = do
      (p, _st) <- withExceptT BranchChooserError
        . flip runMonadChooser_ initialChooserState
        $ getRestOfPath (visitCountsFromList []) startNode (P.start startNode)
      return $ P.build p
  where
    getRestOfPath :: VisitCounts n -> n -> PathBuilder l n -> MonadChooser e s m (PathBuilder l n)
    getRestOfPath visitCounts prevNode prevPath = do
      let visitCounts' = updateVisitCounts prevNode visitCounts
      chooseChild prevNode (getValidSuccs revisitLimit g visitCounts' prevNode)
        >>= \case
          Nothing -> return prevPath
          Just (ChildChoice (_lbl, n) (lbl', n')) ->
            getRestOfPath visitCounts' n $ prevPath -| lbl' |- n'

-- | This allows the caller to modify nodes as they are appended to the path,
-- which is essential for loop unrolling.
-- before it is added to the path. If it returns Nothing, the path will end without the
-- chosen edge/node.
samplePath_
  :: forall s e l n g p m.
  ( Monad m
  , Graph l n g
  , PathConstruct l n p
  , Hashable n
  , Hashable l
  )
  => ChildChooser e s m l n -- Choose between children
  -> s -- initial child chooser state
  -> n
  -> g n
  -> ExceptT (SampleRandomPathError e) m (p n, s)
samplePath_ chooseChild initialChooserState startNode g
  | not (G.hasNode startNode g) = throwError StartNodeNotInGraph
  | otherwise = do
      (p, st) <- withExceptT BranchChooserError
        . flip runMonadChooser_ initialChooserState
        $ getRestOfPath startNode (P.start startNode)
      return (P.build p, st)
  where
    getRestOfPath :: n -> PathBuilder l n -> MonadChooser e s m (PathBuilder l n)
    getRestOfPath prevNode prevPath = do
      chooseChild prevNode (getSuccHalfEdges g prevNode) >>= \case
        Nothing -> return prevPath
        Just choice -> do
          let (_lbl, n) = choice ^. #unmodifiedChoice
              (lbl', n') = choice ^. #modifedChoiceForPathInclusion
          getRestOfPath n $ prevPath -| lbl' |- n'

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

-- | Picks out of list based on double in fst of tuple.
-- The larger the int means greater chance of getting picked.
-- Errors if n's total to zero.  
stochasticChoiceFp
  :: Monad m
  => m Double -- Random double between 0 and 1.0
  -> NonEmpty (Double, x)
  -> m x
stochasticChoiceFp _ ((_, x) :| []) = return x
stochasticChoiceFp randDouble (y :| ys) =
  if total == 0 then
    error "Total sum should not equal zero"
  else do
    r <- randDouble
    return . roulette (y :| ys) $ r * total
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
  | ReqNodeCannotBeReached
  deriving (Eq, Ord, Show, Generic)

-- | This chooses a child randomly, based on descendant count.
chooseChildByDescendantCount
  :: forall m l n s. (Monad m, Hashable n)
  => ((Int, Int) -> m Int)
  -> StrictDescendantsMap n
  -> ChildChooser_ (ChooseChildError n) s m l n
chooseChildByDescendantCount pickFromRange dmap _parentNode childrenHalfEdges = do
  mapM rateChild childrenHalfEdges >>= \case
    [] -> return Nothing
    (x:xs) -> do
      choice <- lift . lift . stochasticChoice pickFromRange $ x :| xs
      return $ Just choice
  where
    rateChild :: (l, n) -> MonadChooser (ChooseChildError n) s m (Int, (l, n))
    rateChild (l, n) = case getStrictDescendants dmap n of
      Nothing -> throwError $ ChildNodeNotFoundInDescendantMap n
      Just s -> return (fromIntegral $ HashSet.size s, (l, n))

-- | This chooses a child randomly, based on descendant count, but it decreases value
-- of each child descendant by how many times it's been visited.
-- This also updates the visit count.
chooseChildRandomly
  :: forall m l n. Monad m
  => ((Int, Int) -> m Int)
  -> ChildChooser_ (ChooseChildError n) () m l n
chooseChildRandomly _ _ [] = return Nothing
chooseChildRandomly pickFromRange _parentNode (x:xs) = do
  choice <- lift . lift . pickFromList pickFromRange $ x :| xs
  return $ Just choice

-- | This chooses a child randomly, based on descendant count, but it decreases value
-- of each child descendant by how many times it's been visited.
-- This also updates the visit count.
chooseChildByLeastVisitedDescendantCount
  :: forall m l n. (Monad m, Hashable n)
  => m Double
  -> StrictDescendantsMap n
  -> ChildChooser_ (ChooseChildError n) (VisitCounts n) m l n
chooseChildByLeastVisitedDescendantCount randDouble dmap parentNode childrenHalfEdges = do
  modify $ updateVisitCounts parentNode
  visitCounts <- get
  mapM (rateChild visitCounts) childrenHalfEdges >>= \case
    [] -> return Nothing
    (x:xs) -> do
      choice <- lift . lift . stochasticChoiceFp randDouble $ x :| xs
      return $ Just choice
  where
    divideByVisitCount :: VisitCounts n -> n -> Double
    divideByVisitCount visitCounts n = 1.0 / fromIntegral (getVisitCount n visitCounts + 1)

    rateChild
      :: VisitCounts n
      -> (l, n)
      -> MonadChooser (ChooseChildError n) (VisitCounts n) m (Double, (l, n))
    rateChild visitCounts (l, n) = case getStrictDescendants dmap n of
      Nothing -> throwError $ ChildNodeNotFoundInDescendantMap n
      Just s -> return ( sum $ divideByVisitCount visitCounts <$> (n : HashSet.toList s)
                       , (l, n)
                       )

data ReqNodesState n
  = InitReqNodes (HashSet n)
  | FindingReqNode (HashSet n)
  | FoundReqNode
  deriving (Eq, Ord, Show, Generic)

-- | This chooses a child randomly, based on descendant count.
-- It only takes paths that reach at least one of the req nodes.
-- If no req nodes can be reached at the start, it throws an exception
chooseChildByDescendantCountAndReqSomeNodes
  :: forall m l n. (Monad m, Hashable n)
  => ((Int, Int) -> m Int)
  -> StrictDescendantsMap n
  -> ChildChooser_ (ChooseChildError n) (ReqNodesState n) m l n
chooseChildByDescendantCountAndReqSomeNodes pickFromRange dmap parentNode childrenHalfEdges = get >>= \case
  InitReqNodes reqSomeNodes -> case getStrictDescendants dmap parentNode of
    Nothing -> throwError $ StartNodeNotFoundInDescendantMap parentNode
    Just descendants -> do
      let reqNodesThatAreDescendants = HashSet.intersection (HashSet.insert parentNode descendants) reqSomeNodes
      if HashSet.null reqNodesThatAreDescendants
        then throwError NoReqNodesCanBeReached
        else do
          put $ FindingReqNode reqNodesThatAreDescendants
          chooseChildByDescendantCountAndReqSomeNodes pickFromRange dmap parentNode childrenHalfEdges
  FindingReqNode reqSomeNodes ->
    if HashSet.member parentNode reqSomeNodes then do
      put FoundReqNode
      -- If parent is found in reqSomeNodes, clear other req nodes, since at least one has been reached.
      chooseChildByDescendantCountAndReqSomeNodes pickFromRange dmap parentNode childrenHalfEdges
    else do
      childrenThatReachSomeNodes <- flip filterM childrenHalfEdges $ \(_, n) -> do
        descs <- liftMaybe (ChildNodeNotFoundInDescendantMap n) $ getStrictDescendants dmap n
        return . not . HashSet.null $ HashSet.intersection reqSomeNodes . HashSet.insert n $ descs
      case childrenThatReachSomeNodes of
        [] -> throwError NoReqNodesCanBeReached
        xs -> do
          put $ FindingReqNode reqSomeNodes
          useDescendantCountChildChooser xs
  FoundReqNode -> useDescendantCountChildChooser childrenHalfEdges
  where
    useDescendantCountChildChooser children = do
      chooseChildByDescendantCount pickFromRange dmap parentNode children >>= \case
        Nothing -> return Nothing
        Just x -> return $ Just x

-- | This chooses a child randomly, based on descendant count.
-- It samples a path along the sequence of nodes.
-- It just ends once it reaches the end of sequence, so if you want a return node,
-- you must include it at the end of the sequence.
-- If it can't reach the next node in the sequence, it throws an exception to abort the path.
chooseChildByDescendantCountAndSequence
  :: forall m l n. (Monad m, Hashable n)
  => ((Int, Int) -> m Int)
  -> StrictDescendantsMap n
  -> ChildChooser_ (ChooseChildError n) [n] m l n
chooseChildByDescendantCountAndSequence pickFromRange dmap parentNode childrenHalfEdges = get >>= \case
  [] -> return Nothing -- all req seq nodes founds
  (req:reqs) -> do
    when (parentNode == req) $ put reqs
    get >>= \case
      [] -> return Nothing -- All reqs found
      reqs' -> do
        let reqsSet = HashSet.fromList reqs'
        childrenThatReachReq <- flip filterM childrenHalfEdges $ \(_, n) -> do
          descs <- liftMaybe (ChildNodeNotFoundInDescendantMap n) $ getStrictDescendants dmap n
          return $ reqsSet `HashSet.isSubsetOf` HashSet.insert n descs
        case childrenThatReachReq of
          [] -> throwError ReqNodeCannotBeReached
          xs -> useDescendantCountChildChooser xs
  where
    useDescendantCountChildChooser children = do
      chooseChildByDescendantCount pickFromRange dmap parentNode children >>= \case
        Nothing -> error "chooseChildByDescendantCount should always succeed"
        Just x -> return $ Just x

data SeqAndVisitCounts n = SeqAndVisitCounts
  { reqSeq :: [n]
  , visitCounts :: VisitCounts n
  } deriving (Eq, Ord, Show, Generic)

chooseChildByVisitedDescendantCountAndSequence
  :: forall m l n. (Monad m, Hashable n)
  => m Double
  -> StrictDescendantsMap n
  -> ChildChooser_ (ChooseChildError n) (SeqAndVisitCounts n) m l n
chooseChildByVisitedDescendantCountAndSequence randDouble dmap parentNode childrenHalfEdges = get >>= \case
  (SeqAndVisitCounts [] _) -> return Nothing -- all req seq nodes found
  (SeqAndVisitCounts (req:reqs) _visitCounts) -> do
    when (parentNode == req) $ #reqSeq .= reqs
    use #reqSeq >>= \case
      [] -> return Nothing -- All reqs found
      reqs' -> do
        let reqsSet = HashSet.fromList reqs'
        childrenThatReachReq <- flip filterM childrenHalfEdges $ \(_, n) -> do
          descs <- liftMaybe (ChildNodeNotFoundInDescendantMap n) $ getStrictDescendants dmap n
          return $ reqsSet `HashSet.isSubsetOf` HashSet.insert n descs
        case childrenThatReachReq of
          [] -> throwError ReqNodeCannotBeReached
          -- xs -> useDescendantCountChildChooser xs
          (x:xs) -> do
            choice <- lift .lift .pickFromListFp randDouble $ x :| xs
            return $ Just choice
  -- where
    -- useDescendantCountChildChooser children = do
    --   withNestedState #visitCounts (chooseChildByLeastVisitedDescendantCount randDouble dmap parentNode children) >>= \case
    --     Nothing -> error "chooseChildByDescendantCount should always succeed"
    --     Just x -> return $ Just x

type SampleRandomPathError' n = SampleRandomPathError (ChooseChildError n)

sampleRandomPath
  :: forall l n g p. (Graph l n g, PathConstruct l n p, Hashable n, Hashable l)
  => StrictDescendantsMap n
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
  -> StrictDescendantsMap n
  -> (Int -> n -> n)
  -> Int
  -> n
  -> g n
  -> HashSet n
  -> m (Either (SampleRandomPathError (ChooseChildError n)) (p n))
sampleRandomPath_ getRandomInRange dmap _changeOnRevisit revisitLimit startNode g reqSomeNodes =
  runExceptT $
  sampleRandomPath_'
  (toFullChooser $ chooseChildByDescendantCountAndReqSomeNodes getRandomInRange dmap)
  (if HashSet.null reqSomeNodes then FoundReqNode else InitReqNodes reqSomeNodes)
  _changeOnRevisit
  revisitLimit
  startNode
  g
