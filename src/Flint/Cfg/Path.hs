module Flint.Cfg.Path where

import Flint.Prelude

import Flint.Types.Query (Query, GetFunction)
import qualified Flint.Cfg.Store as CfgStore
import Flint.Types.Cfg.Store (CfgStore)

import Blaze.Cfg.Path (PilPath)
import qualified Blaze.Cfg.Path as CfgPath
import qualified Blaze.Path as Path
import Blaze.Path (SampleRandomPathError', SampleRandomPathError)
import Blaze.Types.Graph (DescendantsMap)
import Blaze.Types.Function (Function)
import Blaze.Types.Cfg (CallNode, PilNode)
import qualified Blaze.Cfg as Cfg
import Blaze.Types.Pil (Stmt)
import qualified Data.List.NonEmpty as NE

import qualified Data.HashSet as HashSet

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
  -> [Address]
  -> ExplorationStrategy (SampleRandomPathError' PilNode) IO
expandToTargetsStrategy _ _ _ [] = \_ _ -> return Nothing
expandToTargetsStrategy pickFromRange expandDepthLimit store targets = \func currentDepth ->
  if currentDepth > expandDepthLimit then
    return Nothing
  else lift (CfgStore.getFuncCfgInfo store func) >>= \case
    Nothing -> return Nothing
    Just cfgInfo -> do
      let nodesContainingTargets = concatMap (`Cfg.getNodesContainingAddress` (cfgInfo ^. #acyclicCfg)) targets
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
  where
    funcsThatLeadToTargets :: HashSet Function
    funcsThatLeadToTargets = undefined

getCallsFromPath :: PilPath -> [CallNode [Stmt]]
getCallsFromPath = mapMaybe (^? #_Call) . HashSet.toList . Path.nodes

-- | This function explores from the start of a function, expanding calls
--   on a path until it reaches the depth limit.
--   It returns Nothing if the starting func Cfg is not in the store.
exploreForward_
  :: Monad m
  => m UUID
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
        leaveFuncUuid <- lift $ lift genUuid
        return (callNode, innerPath, leaveFuncUuid)
      return
          . Just
          $ foldl (\p (callNode, innerPath, leaveFuncUuid) -> fromJust $
                    CfgPath.expandCall leaveFuncUuid p callNode innerPath)
            mainPath
            innerPaths

-- | Gets a random path from the starting function to the target basic block.
sampleToTarget_
  :: (Int -> IO Int) -- If multiple calls reach target, pick from them.
  -> ((Int, Int) -> IO Int) -- Pick random branch amonst those that reach target
  -> DescendantsMap Function
  -> CfgStore
  -> Function
  -> Address
  -> IO (Maybe PilPath)
sampleToTarget_ pickCallToTarget pickBranch dmap store startingFunc targetAddr = return Nothing

samplesFromQuery_
  :: GetFunction a
  => (Int -> IO Int) -- Pick random target node
  -> ((Int, Int) -> IO Int) -- Pick random branch
  -> CfgStore
  -> Query a
  -> IO [PilPath]
samplesFromQuery_ pickRandomTargetNode pickRandomBranch store q = do
  return []
