module Flint.Cfg.Path where


import Flint.Prelude

import Flint.Types.Query (Query(QueryTarget, QueryExpandAll, QueryExploreDeep, QueryAllPaths), GetFunction(getFunction))
import qualified Flint.Cfg.Store as CfgStore
import Flint.Types.Cfg.Store (CfgStore)
import Flint.Util (incUUID)

import Blaze.Cfg.Path (PilPath)
import qualified Blaze.Cfg.Path as CfgPath
import Blaze.Import.CallGraph (CallGraphImporter)
import qualified Blaze.Path as Path
import Blaze.Path (SampleRandomPathError', SampleRandomPathError)
import Blaze.Types.Graph (DescendantsMap)
import Blaze.Types.Function (Function)
import Blaze.Types.Cfg (CallNode, PilNode)
import qualified Blaze.Cfg as Cfg
import Blaze.Types.Pil (Stmt)
import qualified Data.List.NonEmpty as NE

import qualified Data.HashSet as HashSet
import Data.List (nub)

type CallDepth = Word64
type CallExpansionChooser = Function -> CallDepth -> [CallNode [Stmt]] -> IO [CallNode [Stmt]]

-- | An ExplorationStrategy returns a path and a list of call nodes that,
-- if found in the path, should be expanded and further explored with
-- the same strategy.
-- 
type ExplorationStrategy e m = Function -> CallDepth -> ExceptT e m (Maybe (PilPath, HashSet (CallNode [Stmt])))


-- type ChildChooser s e m l n = s -> n -> NonEmpty (l, n) -> ExceptT e m (Maybe (s, (l, n)))


getCallNodeFunc :: CallNode a -> Maybe Function
getCallNodeFunc n = n ^? #callDest . #_CallFunc

expandAllToDepth :: CallDepth -> CallExpansionChooser
expandAllToDepth expandDepthLimit _func currentDepth nodes
  | currentDepth < expandDepthLimit = return nodes
  | otherwise = return []

-- expandToTargets
--   :: CallDepth
--   -> HashSet Function
--   -> HashSet Function
--   -> CallExpansionChooser
-- expandToTargets expandDepthLimit funcsThatContainTarget funcsThatLeadToTarget currentFunc currentDepth nodes
--   | currentDepth < expandDepthLimit = return []
--   | otherwise = case (HashSet.member

-- | This function explores from the start of a function, expanding calls
--   on a path until it reaches the depth limit.
--   It returns Nothing if the starting func Cfg is not in the store.
exploreFromStartingFunc_
  :: ((Int, Int) -> IO Int) -- Pick random branch
  -> CallExpansionChooser
  -> CallDepth
  -> CfgStore
  -> Function
  -> IO (Maybe PilPath)
exploreFromStartingFunc_ pickFromRange expansionChooser currentDepth store startingFunc = CfgStore.getFuncCfgInfo store startingFunc >>= \case
  Nothing -> return Nothing
  Just cfgInfo -> do
    er <- CfgPath.sampleRandomPath_
      pickFromRange
      (cfgInfo ^. #descendantsMap)
      (cfgInfo ^. #acyclicCfg)
    case er of
      Left err -> error $ "Unexpected sampleRandomPath_ failure: " <> show err
      Right mainPath -> do
        let callsOnPath = mapMaybe (^? #_Call) . HashSet.toList . Path.nodes $ mainPath
        callsToExpand <- expansionChooser startingFunc currentDepth callsOnPath
        innerPaths <- flip mapMaybeM callsToExpand $ \callNode -> runMaybeT $ do
          destFunc <- hoistMaybe $ getCallNodeFunc callNode
          innerPath <- liftMaybeTIO $ exploreFromStartingFunc_
            pickFromRange
            expansionChooser
            (currentDepth + 1)
            store
            destFunc
          leaveFuncUuid <- liftIO randomIO -- TODO: pass in UUID-making func?
          return (callNode, innerPath, leaveFuncUuid)
        return
          . Just
          $ foldl (\p (callNode, innerPath, leaveFuncUuid) -> fromJust $
                    CfgPath.expandCall leaveFuncUuid p callNode innerPath)
            mainPath
            innerPaths

expandAllStrategy
  :: ((Int, Int) -> IO Int)
  -> CallDepth
  -> CfgStore
  -> ExplorationStrategy (SampleRandomPathError' PilNode) IO
expandAllStrategy pickFromRange expandDepthLimit store func currentDepth
  | currentDepth > expandDepthLimit = return Nothing
  | otherwise = lift (CfgStore.getFuncCfgInfo store func) >>= \case
      Nothing -> return Nothing
      Just cfgInfo -> do
        path <- CfgPath.sampleRandomPath_'
          (Path.chooseChildByDescendantCount pickFromRange $ cfgInfo ^. #descendantsMap)
          ()
          (cfgInfo ^. #acyclicCfg)
        return $ Just (path, HashSet.fromList $ cfgInfo ^. #calls)

exploreDeepStrategy
  :: ((Int, Int) -> IO Int)
  -> CallDepth
  -> CfgStore
  -> ExplorationStrategy (SampleRandomPathError' PilNode) IO
exploreDeepStrategy pickFromRange expandDepthLimit store func currentDepth
  | currentDepth > expandDepthLimit = return Nothing
  | otherwise = lift (CfgStore.getFuncCfgInfo store func) >>= \case
      Nothing -> return Nothing
      Just cfgInfo -> do
        path <- CfgPath.sampleRandomPath_'
          (Path.chooseChildByDescendantCount pickFromRange $ cfgInfo ^. #descendantsMap)
          ()
          (cfgInfo ^. #acyclicCfg)
        case getCallsFromPath path of
          [] -> return $ Just (path, HashSet.empty)
          x:xs -> do
            luckyCall <- liftIO . pickFromList pickFromRange $ x :| xs
            return $ Just (path, HashSet.singleton luckyCall)

pickFromList :: Monad m => ((Int, Int) -> m Int) -> NonEmpty a -> m a
pickFromList picker (x :| xs) = do
  n <- picker (0, length xs)
  return $ (x:xs) !! n

-- | This strategy only expands call nodes that reach a target basic block.
-- If multiple calls reach the target, it randomly chooses one.
-- If the target is within the Cfg itself and through calls, it randomly chooses
-- between reaching the target in the current function, or pursuing a call.
expandToTargetsStrategy
  :: ((Int, Int) -> IO Int)
  -> CallDepth
  -> CfgStore
  -> HashSet Function
  -> NonEmpty Address
  -> ExplorationStrategy (SampleRandomPathError' PilNode) IO
expandToTargetsStrategy pickFromRange expandDepthLimit store funcsThatLeadToTargets targets func currentDepth =
  if currentDepth > expandDepthLimit then
    return Nothing
  else lift (CfgStore.getFuncCfgInfo store func) >>= \case
    Nothing -> return Nothing
    Just cfgInfo -> do
      let nodesContainingTargets = concatMap (`Cfg.getNodesContainingAddress` (cfgInfo ^. #acyclicCfg)) . NE.toList $ targets
          callNodesThatLeadToTargets :: [CallNode [Stmt]]
          callNodesThatLeadToTargets
            = filter (maybe False (`HashSet.member` funcsThatLeadToTargets) . getCallNodeFunc)
            $ cfgInfo ^. #calls
          combined = (Left <$> nodesContainingTargets) <> (Right <$> callNodesThatLeadToTargets)
      case NE.nonEmpty combined of
        Nothing -> return Nothing
        Just ne -> do
          choice <- lift (pickFromList pickFromRange ne)
          let (targetNode, expandLater) = case choice of
                Left targetNode' -> (targetNode', HashSet.empty)
                Right targetCallNode -> ( Cfg.Call targetCallNode
                                        , HashSet.singleton targetCallNode
                                        )
          path <- CfgPath.sampleRandomPath_'
            (Path.chooseChildByDescendantCountAndReqSomeNodes
              pickFromRange
              $ cfgInfo ^. #descendantsMap)
            (Path.InitReqNodes $ HashSet.singleton targetNode)
            (cfgInfo ^. #acyclicCfg)
          return $ Just (path, expandLater)              

mkExpandToTargetsStrategy
  :: ((Int, Int) -> IO Int)
  -> CallDepth
  -> CfgStore
  -> NonEmpty (Function, Address)
  -> IO (ExplorationStrategy (SampleRandomPathError' PilNode) IO)
mkExpandToTargetsStrategy pickFromRange expandDepthLimit store targets = do
  funcsThatLeadToTargets <- foldM addAncestors HashSet.empty . fmap fst $ targets
  return $ expandToTargetsStrategy pickFromRange expandDepthLimit store funcsThatLeadToTargets (snd <$> targets)
  where
    addAncestors :: HashSet Function -> Function -> IO (HashSet Function)
    addAncestors s func = do
      ancestors <- CfgStore.getAncestors store func >>= \case
        Nothing -> error $ "Could not find func ancestors for " <> show func
        Just ancestors -> return ancestors
      return . HashSet.insert func $ HashSet.union ancestors s
      


getCallsFromPath :: PilPath -> [CallNode [Stmt]]
getCallsFromPath = mapMaybe (^? #_Call) . HashSet.toList . Path.nodes

-- | This function explores from the start of a function, expanding calls
--   on a path until it reaches the depth limit.
--   It returns Nothing if the starting func Cfg is not in the store.
exploreForward_
  :: Monad m
  => (UUID -> m UUID) -- generate a new UUID, possibly based on Call node uuid.
  -> ExplorationStrategy e m
  -> CallDepth
  -> Function
  -> ExceptT e m (Maybe PilPath)
exploreForward_ genUuid exploreStrat currentDepth startingFunc = do
  exploreStrat startingFunc currentDepth >>= \case
    Nothing -> return Nothing
    Just (mainPath, callsToExpand) -> do
      let callsOnPath = mapMaybe (^? #_Call) . HashSet.toList . Path.nodes $ mainPath
          callsToExpand' = filter (`HashSet.member` callsToExpand) callsOnPath
      innerPaths <- flip mapMaybeM callsToExpand' $ \callNode -> runMaybeT $ do
        destFunc <- hoistMaybe $ getCallNodeFunc callNode
        innerPath <- hoistMaybeM $ exploreForward_
          genUuid
          exploreStrat
          (currentDepth + 1)
          destFunc
        leaveFuncUuid <- lift . lift . genUuid . Cfg.getNodeUUID . Cfg.Call $ callNode
        return (callNode, innerPath, leaveFuncUuid)
      return
          . Just
          $ foldl (\p (callNode, innerPath, leaveFuncUuid) -> fromJust $
                    CfgPath.expandCall leaveFuncUuid p callNode innerPath)
            mainPath
            innerPaths

-- -- | Gets a random path from the starting function to the target basic block.
-- sampleToTarget_
--   :: (Int -> IO Int) -- If multiple calls reach target, pick from them.
--   -> ((Int, Int) -> IO Int) -- Pick random branch amonst those that reach target
--   -> DescendantsMap Function
--   -> CfgStore
--   -> Function
--   -> Address
--   -> IO (Maybe PilPath)
-- sampleToTarget_ pickCallToTarget pickBranch dmap store startingFunc targetAddr = return Nothing

-- | Gets samples for a Query.
samplesFromQuery
  :: CfgStore
  -> Query Function
  -> IO [PilPath]
samplesFromQuery store = \case
  QueryTarget opts -> do
    strat <- mkExpandToTargetsStrategy randomRIO (opts ^. #callExpandDepthLimit) store (opts ^. #mustReachSome)
    let action = exploreForward_ incUUID' strat 0 (opts ^. #start)
    collectSamples (opts ^. #numSamples) action

  QueryExpandAll opts -> do
    let strat = expandAllStrategy randomRIO (opts ^. #callExpandDepthLimit) store
        action = exploreForward_ incUUID' strat 0 (opts ^. #start)
    collectSamples (opts ^. #numSamples) action

  QueryExploreDeep opts -> do
    let strat = exploreDeepStrategy randomRIO (opts ^. #callExpandDepthLimit) store
        action = exploreForward_ incUUID' strat 0 (opts ^. #start)
    collectSamples (opts ^. #numSamples) action

  QueryAllPaths opts -> CfgStore.getFuncCfgInfo store (opts ^. #start) >>= \case
    Nothing -> error $ "Could not get cfg for function " <> show (opts ^. #start)
    Just cfgInfo -> return . CfgPath.getAllSimplePaths $ cfgInfo ^. #acyclicCfg

  where
    incUUID' = return . incUUID
    collectSamples :: forall e. Show e => Word64 -> ExceptT e IO (Maybe PilPath) -> IO [PilPath]
    collectSamples n action =
      fmap (nub . catMaybes) . replicateConcurrently (fromIntegral n) $ runExceptT action >>= \case
        Left err -> do
          putText $ "ERROR getting sample: " <> show err
          return Nothing
        Right mpath -> return mpath
        
