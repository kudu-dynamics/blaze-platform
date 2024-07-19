{- HLINT ignore "Use if" -}

module Flint.Cfg.Path where

import Flint.Prelude

import qualified Flint.Types.CachedCalc as CC
import Flint.Types.Query (Query(QueryTarget, QueryExpandAll, QueryExploreDeep, QueryAllPaths, QueryCallSeq), CallSeqPrep)
import qualified Flint.Cfg.Store as CfgStore
import Flint.Types.Cfg.Store (CfgStore)
import Flint.Util (incUUID)

import qualified Blaze.Cfg.Interprocedural as InterCfg
import Blaze.Cfg.Path (PilPath, SequenceChooserState, initSequenceChooserState, samplePathContainingSequence_)
import qualified Blaze.Cfg.Path as CfgPath
import Blaze.Graph (StrictDescendantsMap, Route)
import qualified Blaze.Graph as G
import qualified Blaze.Path as Path
import Blaze.Path (SampleRandomPathError', pickFromListFp)
import Blaze.Pretty (pretty', FullCfNode(FullCfNode))
import Blaze.Types.Function (Function)
import Blaze.Types.Cfg (CallNode, PilCfg, PilNode)
import qualified Blaze.Cfg as Cfg
import Blaze.Types.Pil (Stmt)

import qualified Data.List.NonEmpty as NE

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.List (nub)

type CallDepth = Word64
type CallExpansionChooser = Function -> CallDepth -> [CallNode [Stmt]] -> IO [CallNode [Stmt]]

-- | An ExplorationStrategy returns a path and a list of call nodes that,
-- if found in the path, should be expanded and further explored with
-- the same strategy.
-- 
data ExpandCall s = ExpandCall
  { newState :: s
  -- TODO: truncateAfter True would mean that path after this call is cut off
  -- , truncateAfter :: Bool
  , callSite :: CallNode [Stmt]
  } deriving (Eq, Ord, Show, Generic, Hashable)

type ExplorationStrategy s e m = Function -> s -> ExceptT e m (Maybe (PilPath, HashSet (ExpandCall s)))

getCallNodeFunc :: CallNode a -> Maybe Function
getCallNodeFunc n = n ^? #callDest . #_CallFunc

expandAllToDepth :: CallDepth -> CallExpansionChooser
expandAllToDepth expandDepthLimit _func currentDepth nodes
  | currentDepth < expandDepthLimit = return nodes
  | otherwise = return []

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
exploreFromStartingFunc_ pickFromRange expansionChooser currentDepth store startingFunc = CfgStore.getFreshFuncCfgInfo store startingFunc >>= \case
  Nothing -> return Nothing
  Just cfgInfo -> do
    er <- CfgPath.sampleRandomPath_
      pickFromRange
      (cfgInfo ^. #strictDescendantsMap)
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
  -> ExplorationStrategy CallDepth (SampleRandomPathError' PilNode) IO
expandAllStrategy _pickFromRange expandDepthLimit store func currentDepth
  | currentDepth > expandDepthLimit = return Nothing
  | otherwise = lift (CfgStore.getFreshFuncCfgInfo store func) >>= \case
      Nothing -> return Nothing
      Just cfgInfo -> do
        -- path <- CfgPath.sampleRandomPath_'
        --   (Path.toFullChooser . Path.chooseChildByDescendantCount pickFromRange $ cfgInfo ^. #strictDescendantsMap)
        --   ()
        --   (cfgInfo ^. #acyclicCfg)
        let rootNode = Cfg.getRootNode $ cfgInfo ^. #cfg
            retNodes :: [PilNode]
            retNodes = fmap (Cfg.BasicBlock . view #basicBlock) . InterCfg.getRetNodes $ cfgInfo ^. #cfg
        case retNodes of
          [] -> return Nothing -- TODO: shouldn't be error, should just snip
          (r:rs) -> do
            retNode <- pickFromList randomRIO $ r :| rs
            let seqState = initSequenceChooserState [retNode]
            rpath <- liftIO $ CfgPath.samplePathContainingSequenceIO
                     (cfgInfo ^. #strictDescendantsMap)
                     seqState
                     rootNode
                     (cfgInfo ^. #cfg)
            case rpath of
              Left err -> do
                error $ "Error sampling path: " <> show err
                -- return Nothing -- TODO: should this be error?
              Right (path, _sst) -> do
                -- TODO: only expand calls that are actually in path.
                let expCalls = fmap (ExpandCall (currentDepth + 1))
                               . mapMaybe (^? #_Call)
                               . HashSet.toList
                               . Path.nodes
                               $ path
                -- let expCalls = ExpandCall (currentDepth + 1) <$> cfgInfo ^. #calls
                return $ Just (path, HashSet.fromList expCalls)

exploreDeepStrategy
  :: ((Int, Int) -> IO Int)
  -> CallDepth
  -> CfgStore
  -> ExplorationStrategy CallDepth (SampleRandomPathError' PilNode) IO
exploreDeepStrategy pickFromRange expandDepthLimit store func currentDepth
  | currentDepth > expandDepthLimit = return Nothing
  | otherwise = lift (CfgStore.getFreshFuncCfgInfo store func) >>= \case
      Nothing -> return Nothing
      Just cfgInfo -> do
        path <- CfgPath.sampleRandomPath_'
          (Path.toFullChooser . Path.chooseChildByDescendantCount pickFromRange $ cfgInfo ^. #strictDescendantsMap)
          ()
          (cfgInfo ^. #acyclicCfg)
        case getCallsFromPath path of
          [] -> return $ Just (path, HashSet.empty)
          x:xs -> do
            luckyCall <- liftIO . pickFromList pickFromRange $ x :| xs
            return $ Just (path, HashSet.singleton $ ExpandCall (currentDepth + 1) luckyCall)

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
  -> ExplorationStrategy CallDepth (SampleRandomPathError' PilNode) IO
expandToTargetsStrategy pickFromRange expandDepthLimit store funcsThatLeadToTargets targets func currentDepth =
  if currentDepth > expandDepthLimit then
    return Nothing
  else lift (CfgStore.getFreshFuncCfgInfo store func) >>= \case
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
                                        , HashSet.singleton $ ExpandCall (currentDepth + 1) targetCallNode
                                        )
          path <- CfgPath.sampleRandomPath_'
            (Path.toFullChooser . Path.chooseChildByDescendantCountAndReqSomeNodes
              pickFromRange
              $ cfgInfo ^. #strictDescendantsMap)
            (Path.InitReqNodes $ HashSet.singleton targetNode)
            (cfgInfo ^. #acyclicCfg)
          return $ Just (path, expandLater)

mkExpandToTargetsStrategy
  :: ((Int, Int) -> IO Int)
  -> CallDepth
  -> CfgStore
  -> NonEmpty (Function, Address)
  -> IO (ExplorationStrategy CallDepth (SampleRandomPathError' PilNode) IO)
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

-- | For now, we take the indolent approach and use ExpandToTarget
-- to reach a callsite for the last function in the CallSeq, ignoring all
-- the functions that may occur before.
-- Returns Nothing if there are no calls to the last func in the CallSeq.
mkExpandAlongCallSeqStrategy
  :: ((Int, Int) -> IO Int)
  -> CallDepth
  -> CfgStore
  -> CallSeqPrep
  -> IO (Maybe (ExplorationStrategy CallDepth (SampleRandomPathError' PilNode) IO))
mkExpandAlongCallSeqStrategy pickFromRange expandDepthLimit store prep = do
  CC.get (prep ^. #lastCall) (store ^. #callSitesCache) >>= \case
    Nothing -> error $ "Could not find func call sites cache for " <> show (prep ^. #lastCall)
    Just [] -> return Nothing
    Just (y:ys) -> do
      let targets = fmap (\x -> (x ^. #caller, x ^. #address)) $ y :| ys
      funcsThatLeadToTargets <- foldM addAncestors HashSet.empty . fmap fst $ targets
      return . Just $ expandToTargetsStrategy pickFromRange expandDepthLimit store funcsThatLeadToTargets (snd <$> targets)
  where
    addAncestors :: HashSet Function -> Function -> IO (HashSet Function)
    addAncestors s func = do
      ancestors <- CfgStore.getAncestors store func >>= \case
        Nothing -> error $ "Could not find func ancestors for " <> show func
        Just ancestors -> return ancestors
      return . HashSet.insert func $ HashSet.union ancestors s

-- | For now, we take the indolent approach and use ExpandToTarget
-- to reach a callsite for the last function in the CallSeq, ignoring all
-- the functions that may occur before.
-- Returns Nothing if there are no calls to the last func in the CallSeq.
mkFanAlongCallSeqStrategy
  :: ((Int, Int) -> IO Int)
  -> CallDepth
  -> CfgStore
  -> CallSeqPrep
  -> IO (Maybe (ExplorationStrategy CallDepth (SampleRandomPathError' PilNode) IO))
mkFanAlongCallSeqStrategy pickFromRange expandDepthLimit store prep = do
  mtargets <- fmap NE.nonEmpty . flip concatMapM (NE.toList $ prep ^. #callSeq) $ \func -> do
    CC.get func (store ^. #callSitesCache) >>= \case
      Nothing -> error $ "Could not find func call sites cache for " <> show (prep ^. #lastCall)
      Just xs -> return $ (\x -> (x ^. #caller, x ^. #address)) <$> xs
  case mtargets of
    Nothing -> return Nothing
    Just targets -> do
      funcsThatLeadToTargets <- foldM addAncestors HashSet.empty . fmap fst $ targets
      return . Just $ expandToTargetsStrategy pickFromRange expandDepthLimit store funcsThatLeadToTargets (snd <$> targets)
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
  -> ExplorationStrategy s e m
  -> s
  -> Function
  -> ExceptT e m (Maybe PilPath)
exploreForward_ genUuid exploreStrat st startingFunc = do
  exploreStrat startingFunc st >>= \case
    Nothing -> return Nothing
    Just (mainPath, callsToExpand) -> do
      let callsOnPath = HashSet.fromList
            . mapMaybe (^? #_Call)
            . HashSet.toList
            . Path.nodes
            $ mainPath
          callsToExpand' = filter ((`HashSet.member` callsOnPath) . view #callSite)
            . HashSet.toList
            $ callsToExpand
      innerPaths <- flip mapMaybeM callsToExpand' $ \expCall -> runMaybeT $ do
        let callNode = expCall ^. #callSite
        destFunc <- hoistMaybe $ getCallNodeFunc callNode
        innerPath <- hoistMaybeM $ exploreForward_
          genUuid
          exploreStrat
          (expCall ^. #newState)
          destFunc

        let changeNodeId n = do
              let uuid = Cfg.getNodeUUID n
              uuid' <- genUuid uuid
              return $ Cfg.setNodeUUID uuid' n
        -- This is really dirty, but because we are pulling from a cache instead of
        -- generating Cfgs every time, we can run into node id conflicts when a path
        -- contains two paths from the same expanded function.
        -- TODO: do this at the frontier when getting CfgInfo so it's more efficient
        --       (here the ids will be updated multiple times for an inner path)
        innerPath' <- lift . lift $ CfgPath.safeTraverse changeNodeId innerPath
        
        leaveFuncUuid <- lift . lift . genUuid . Cfg.getNodeUUID . Cfg.Call $ callNode
        return (callNode, innerPath', leaveFuncUuid)

      return
        . Just
        $ foldl
        (\p (callNode, innerPath, leaveFuncUuid) ->
            case CfgPath.expandCall leaveFuncUuid p callNode innerPath of
              Nothing -> error
                $  "\n\nFailed to expand call:\n " <> cs (pretty' $ FullCfNode . Cfg.Call $ callNode)
                <> "\n\nOuter path:\n" <> cs (pretty' $ FullCfNode <$> p)
                <> "\n\nInner Path:\n" <> cs (pretty' $ FullCfNode <$> innerPath)
              Just p' -> p'
        )
        mainPath
        innerPaths

-- | Gets samples for a Query.
samplesFromQuery
  :: CfgStore
  -> Function
  -> Query Function
  -> IO [PilPath]
samplesFromQuery store startFunc = \case
  QueryTarget opts -> do
    strat <- mkExpandToTargetsStrategy randomRIO (opts ^. #callExpandDepthLimit) store (opts ^. #mustReachSome)
    let action = exploreForward_ incUUID' strat 0 startFunc
    collectSamples (opts ^. #numSamples) action

  QueryExpandAll opts -> do
    let strat = expandAllStrategy randomRIO (opts ^. #callExpandDepthLimit) store
        action = exploreForward_ incUUID' strat 0 startFunc
    collectSamples (opts ^. #numSamples) action

  QueryExploreDeep opts -> do
    let strat = exploreDeepStrategy randomRIO (opts ^. #callExpandDepthLimit) store
        action = exploreForward_ incUUID' strat 0 startFunc
    collectSamples (opts ^. #numSamples) action

  QueryAllPaths -> CfgStore.getFuncCfgInfo store startFunc >>= \case
    Nothing -> error $ "Could not get cfg for function " <> show startFunc
    Just cfgInfo -> return . CfgPath.getAllSimplePaths $ cfgInfo ^. #acyclicCfg

  QueryCallSeq opts -> do
    mkFanAlongCallSeqStrategy randomRIO (opts ^. #callExpandDepthLimit) store (opts ^. #callSeqPrep) >>= \case
      Nothing -> return [] -- TODO: this currently happens when there are no call sites for last func in CallSeq
      Just strat -> do
        let action = exploreForward_ incUUID' strat 0 startFunc
        collectSamples (opts ^. #numSamples) action

  where
    incUUID' = return . incUUID
    collectSamples :: forall e. Show e => Word64 -> ExceptT e IO (Maybe PilPath) -> IO [PilPath]
    collectSamples n action =
      fmap (nub . catMaybes) . replicateConcurrently (fromIntegral n) $ runExceptT action >>= \case
        Left err -> do
          putText $ "ERROR getting sample: " <> show err
          return Nothing
        Right mpath -> return mpath

data SampleFromRouteState = SampleFromRouteState
  { remaningRoute :: Route Function PilNode
  , sequenceChooserState :: SequenceChooserState
  } deriving (Eq, Ord, Show, Generic)

-- | Returns True of node returns control flow to caller
isReturnNode :: PilNode -> Bool
isReturnNode (Cfg.BasicBlock bb) = isJust $ Cfg.parseReturnNode bb
isReturnNode _ = False

sampleFromRoute_
  :: forall m. Monad m
  => SequenceChooserState
  -> m Double
  -> m UUID
  -> HashMap Function PilCfg
  -> HashMap Function (StrictDescendantsMap PilNode) -- dmaps of local cfg nodes
  -> Function -- start func
  -> PilNode -- start node
  -> Route Function PilNode
  -> m (Maybe (PilPath, (Route Function PilNode, SequenceChooserState)))
sampleFromRoute_ _ _ _ _ _ _ _ [] = return Nothing
sampleFromRoute_ seqState randDouble randUUID cfgs dmaps currentFunc currentNode (paction:pactions) =
  case paction of
    G.Finished -> return Nothing
    G.InnerNode n -> do
      -- TODO: maybe this should return Nothing instead of throwing an error?
      let dmap = fromMaybe (error $ "Couldn't find " <> show currentFunc <> " in dmaps")
                 $ HashMap.lookup currentFunc dmaps
          cfg = fromMaybe (error $ "Couldn't find " <> show currentFunc <> " in cfgs")
                $ HashMap.lookup currentFunc cfgs
          seqState' = seqState & #seqAndVisitCounts . #reqSeq .~ [currentNode, n]
      samplePathContainingSequence_ randDouble randUUID dmap seqState' currentNode cfg >>= \case
        Left err -> error $ show err
        Right (p, seqState'') -> do
          r <- sampleFromRoute_ seqState'' randDouble randUUID cfgs dmaps currentFunc n pactions
          case r of
            -- Rest of route "Finished", hopefully...
            Nothing -> return $ Just (p, ([], seqState'')) -- or return Nothing?
            Just (p', (route', seqState''')) ->
              case Path.connect p p' of
                Nothing -> error "Could not connect paths"
                Just p'' -> return $ Just (p'', (route', seqState'''))
    G.EnterContext callNode destFunc -> do
      let dmap = fromMaybe (error $ "Couldn't find " <> show currentFunc <> " in dmaps")
            $ HashMap.lookup currentFunc dmaps
          cfg = fromMaybe (error $ "Couldn't find " <> show currentFunc <> " in cfgs")
            $ HashMap.lookup currentFunc cfgs
          destCfg = fromMaybe (error $ "Couldn't find dest func " <> show currentFunc <> " in cfgs")
            $ HashMap.lookup destFunc cfgs
          asCallNode = case callNode of
            Cfg.Call x -> x
            _ -> error "Expected call node to be call node"
          seqState' = seqState & #seqAndVisitCounts . #reqSeq .~ [currentNode, callNode]

      -- Get path from current node to call node
      samplePathContainingSequence_ randDouble randUUID dmap seqState' currentNode cfg >>= \case
        Left err -> error $ show err
        Right (pathToCallNode, seqStateAfterCallNodeReached) -> do

          -- Get inner path and eat up some route actions
          r <- sampleFromRoute_ (initSequenceChooserState []) randDouble randUUID cfgs dmaps destFunc (Cfg.getRootNode destCfg) pactions
          case r of
            Nothing -> return Nothing -- failed to get inner path
            Just (innerPath, (routeAfterInnerExpansion, seqStateAfterInnerExpansion)) -> do
              (innerPath' :: PilPath) <- flip CfgPath.safeTraverse innerPath $ \n -> do
                uuid <- randUUID
                return $ Cfg.setNodeUUID uuid n
              leaveFuncUUID <- randUUID
              case CfgPath.expandCall leaveFuncUUID pathToCallNode asCallNode innerPath' of
                Nothing -> error $ "Failed to expand call node:\n " <> show asCallNode
                Just pathWithExpandedCall -> do
                  case routeAfterInnerExpansion of
                    -- No more routes. This means the path was cut off inside
                    -- the call and there is no LeaveFuncNode
                    [] -> return $ Just (pathWithExpandedCall, ([], seqStateAfterCallNodeReached))
                    routez -> do
                      let lastNode = Path.end pathWithExpandedCall
                      case Cfg.getNodeUUID lastNode == leaveFuncUUID of
                        False -> error $ "Expected last node of expanded path to be LeaveFuncNode" <> ". truth b, there wuz these routez: " <> show routez
                        True -> do
                          -- Get rest of path from callNode.
                          sampleFromRoute_ seqStateAfterCallNodeReached randDouble randUUID cfgs dmaps currentFunc callNode routeAfterInnerExpansion >>= \case
                            Nothing -> return $ Just (pathWithExpandedCall, (routeAfterInnerExpansion, seqStateAfterInnerExpansion)) -- or return Nothing?
                            Just (pathFromCallNode, (finalRoute, finalSeqState)) -> do
                              let finalPath
                                    = Path.append
                                      pathWithExpandedCall Cfg.UnconditionalBranch
                                      $ Path.drop 1 pathFromCallNode
                              return $ Just (finalPath, (finalRoute, finalSeqState))
    G.ExitContext _ -> do
      -- TODO: maybe this should return Nothing instead of throwing an error?
      let dmap = fromMaybe (error $ "Couldn't find " <> show currentFunc <> " in dmaps")
                 $ HashMap.lookup currentFunc dmaps
          cfg = fromMaybe (error $ "Couldn't find " <> show currentFunc <> " in cfgs")
                $ HashMap.lookup currentFunc cfgs
          rets = filter isReturnNode . HashSet.toList . Cfg.nodes $ cfg
      case rets of
        [] -> return Nothing
        (ret:rets') -> do
          pickedRet <- pickFromListFp randDouble $ ret :| rets'
          let seqState' = seqState & #seqAndVisitCounts . #reqSeq .~ [currentNode, pickedRet]
          samplePathContainingSequence_ randDouble randUUID dmap seqState' currentNode cfg >>= \case
            Left _err ->
              -- TODO: I think this function is overloading Nothing, using it both for an
              -- "abort-everything" and for a "route finished"
              -- Either pick one use for it or make a new type with 3 options
              return Nothing -- Sometimes no ret node can be reached
            Right (p, seqState'') -> return $ Just (p, (pactions, seqState''))

sampleFromRoute
  :: forall m. Monad m
  => m Double
  -> m UUID
  -> HashMap Function PilCfg
  -> HashMap Function (StrictDescendantsMap PilNode) -- dmaps of local cfg nodes
  -> Function -- start func
  -> PilNode -- start node
  -> Route Function PilNode
  -> m (Maybe (PilPath, (Route Function PilNode, SequenceChooserState)))
sampleFromRoute = sampleFromRoute_ $ initSequenceChooserState []

sampleFromRouteIO
  :: HashMap Function PilCfg
  -> HashMap Function (StrictDescendantsMap PilNode) -- dmaps of local cfg nodes
  -> Function -- start func
  -> PilNode -- start node
  -> Route Function PilNode
  -> IO (Maybe (PilPath, (Route Function PilNode, SequenceChooserState)))
sampleFromRouteIO = sampleFromRoute randomIO randomIO
                 
