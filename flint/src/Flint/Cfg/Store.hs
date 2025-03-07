{- HLINT ignore "Redundant <$>" -}

module Flint.Cfg.Store where

import Flint.Prelude

import Flint.Types.Cfg.Store
import qualified Flint.Types.CachedCalc as CC

import qualified Blaze.CallGraph as CG
import qualified Blaze.Cfg.Interprocedural as CfgI
import Blaze.Cfg.Path (makeCfgAcyclic)
import Blaze.Graph (RouteMakerCtx(RouteMakerCtx))
import qualified Blaze.Graph as G
import qualified Blaze.Persist.Db as Db
import Blaze.Types.Function (Function)
import Blaze.Types.Graph (calcDescendantsMap, calcStrictDescendantsMap, DescendantsMap(DescendantsMap), StrictDescendantsMap(StrictDescendantsMap))
import qualified Blaze.Types.Pil as Pil

import qualified Blaze.Import.CallGraph as CG
import Blaze.Import.CallGraph (CallGraphImporter)
import qualified Blaze.Import.Cfg as ImpCfg
import Blaze.Import.Cfg (CfgImporter, NodeDataType)

import Blaze.Types.CallGraph (CallGraph)
import Blaze.Types.Cfg (PilCfg, PilNode)
import qualified Blaze.Types.Cfg as Cfg
import Blaze.Util (getMemoized)

import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap




-- This is a cache of Cfgs for functions.
-- This version only supports functions from a single binary.

-- If you have a bunch of object files, you can collect them like so:
-- gcc -no-pie -Wl,--unresolved-symbols=ignore-all -o full_collection_binary *.o
-- OR you can maybe use this one:
-- ld -r -o combined.o *.o

init
  :: ( CallGraphImporter imp
     , NodeDataType imp ~ PilNode
     , CfgImporter imp
     )
  => Maybe FilePath -> imp -> IO CfgStore
init mDbFilePath imp = do
  mDb <- case mDbFilePath of
    Nothing -> return Nothing
    Just fp -> Just <$> Db.init fp
  store <- CfgStore
    <$> atomically CC.create
    <*> atomically CC.create
    <*> atomically CC.create
    <*> atomically CC.create
    <*> atomically CC.create
    <*> atomically CC.create
    <*> atomically CC.create

  allFuncCfgs <- getFuncsWithCfgs imp
  let funcCfgsSansPltThunks = purgeInternalPltThunks allFuncCfgs
      funcCfgsMapping = HashMap.fromList funcCfgsSansPltThunks
      callNodesMapping = getCallNodesMapping funcCfgsSansPltThunks
      internalCallDests :: HashMap Function (HashSet Function)
      internalCallDests = fmap ( HashSet.fromList
                                . mapMaybe getConcreteFuncCallDest)
                          callNodesMapping
      internalFuncs = fst <$> funcCfgsSansPltThunks

  CC.setCalc () (store ^. #funcs) $ return internalFuncs
  CC.setCalc () (store ^. #callGraphCache) $ do
    let getCG = return $ calcCallGraphFromInternalCallDests internalCallDests
    case mDb of
      Nothing -> getCG
      Just db -> Db.loadCallGraph db >>= \case
        Nothing -> do
          putText "Creating new call graph"
          cg <- getCG
          Db.insertCallGraph db cg
          putText $ "Stored CallGraph in db " <> show (length (show cg :: String))
          return cg
        Just cg -> do
          putText $ "Retrieved CallGraph from db " <> show (length (show cg :: String))
          return cg
  CC.setCalc () (store ^. #transposedCallGraphCache) $ do
    cg <- getCallGraph store
    return $ G.transpose cg
  initialFuncs <- CC.get_ $ store ^. #funcs
  -- Set up calcs for ancestors
  forM_ initialFuncs $ \func -> do
    CC.setCalc func (store ^. #ancestorsCache) $ do
      cg <- fromJust <$> CC.get () (store ^. #transposedCallGraphCache)
      return $ G.getStrictDescendants func cg
    CC.setCalc func (store ^. #descendantsCache) $ do
      cg <- fromJust <$> CC.get () (store ^. #callGraphCache)
      return $ G.getStrictDescendants func cg
    CC.setCalc func (store ^. #callSitesCache) $ do
      case HashMap.lookup func callNodesMapping of
        Nothing -> return []
        Just dests -> return $ mapMaybe toCallSite dests
          where
            toCallSite n = case n ^. #callDest of
              Pil.CallFunc destFunc -> Just
                $ CG.CallSite func (n ^. #start) (CG.DestFunc destFunc)
              _ -> Nothing
    addFunc' store func $ HashMap.lookup func funcCfgsMapping
  return store

-- | Returns a list of all the functions in the binary that we can make a Cfg for.
getFuncsWithCfgs
  :: ( CallGraphImporter a
     , CfgImporter a
     , NodeDataType a ~ PilNode
     )
  => a -> IO [(Function, PilCfg)]
getFuncsWithCfgs imp = do
  funcs <- CG.getFunctions imp
  fmap catMaybes . forM funcs $ \func -> fmap (func,) <$> ImpCfg.getCfg_ imp func 0

data PltThunkMapping = PltThunkMapping
  { renamedFunc :: Function
  , newCallDest :: Pil.CallDest Pil.Expression
  } deriving (Eq, Ord, Show, Generic)

-- | our heuristic for finding a Plt thunk is that the CFG has a single block
-- with a single tailcall with a ConstFuncPtr target to func of same name
-- Returns Nothing if function is apparently not a Plt thunk
getPltThunkDest :: Function -> PilCfg -> Maybe (Pil.CallDest Pil.Expression)
getPltThunkDest srcFunc cfg = case HashSet.size nodes of
  2 -> case (getNodeStatements rootNode, getNodeStatements <$> HashSet.toList nonRootNodes) of
         ( [ Pil.Def (Pil.DefOp v (Pil.Expression _ (Pil.CALL (Pil.CallOp dest@(Pil.CallAddr (Pil.ConstFuncPtrOp _ (Just x))) _ _)))) ]
           , [[ Pil.Ret (Pil.RetOp (Pil.Expression _ (Pil.VAR (Pil.VarOp v')))) ]]) ->
           if v == v' && x == srcFunc ^. #name
           then Just dest
           else Nothing
         ( [ Pil.Def (Pil.DefOp v (Pil.Expression _ (Pil.CALL (Pil.CallOp dest@(Pil.CallFunc destFunc) _ _)))) ]
           , [[ Pil.Ret (Pil.RetOp (Pil.Expression _ (Pil.VAR (Pil.VarOp v')))) ]]) ->
           if v == v' && destFunc ^. #name == srcFunc ^. #name
           then Just dest
           else Nothing
         _ -> Nothing
  _ -> Nothing
  where
    getNodeStatements = fmap (view #statement) . Cfg.getNodeData
    rootNode = Cfg.getRootNode cfg
    nodes = G.nodes cfg
    nonRootNodes = HashSet.delete rootNode nodes

-- | Finds all the PLT thunks and possibly the real funcs they point to
-- The result is map where:
-- key: original Function object that is actually a thunk
-- val: new _renamed function and CallDest that should replace calls to this thunk
-- The CallDest will be a `CallAddr ConstFuncPtrOp` for externs and will be
-- a `CallFunc Function` for thunks that point internally.
getPltThunkMapping :: [(Function, PilCfg)] -> HashMap Function PltThunkMapping
getPltThunkMapping allFuncCfgs = HashMap.fromList . fmap f $ pltFuncs
  where
    f :: (Function, Pil.CallDest Pil.Expression)
      -> (Function, PltThunkMapping)
    f (thunkFunc, thunkDest) =
      ( thunkFunc
      , PltThunkMapping
        { renamedFunc = thunkFunc & #name %~ ("_" <>)
        , newCallDest = maybe thunkDest Pil.CallFunc
          $ HashMap.lookup (thunkFunc ^. #name) allFuncsSansPltsByName
        }
      )

    pltFuncs :: [(Function, Pil.CallDest Pil.Expression)]
    pltFuncs = flip mapMaybe allFuncCfgs $ \(func, cfg) -> do
      (func,) <$> getPltThunkDest func cfg

    pltFuncSet :: HashSet Function
    pltFuncSet = HashSet.fromList . fmap fst $ pltFuncs

    allFuncsSansPlts :: [(Function, PilCfg)]
    allFuncsSansPlts = filter (not . (`HashSet.member` pltFuncSet) . fst) allFuncCfgs

    -- TODO: Should we throw an error if there are two funcs with the same name?
    -- presumably, without the PLT thunk funcs, all func names should be unique.
    allFuncsSansPltsByName :: HashMap Text Function
    allFuncsSansPltsByName = HashMap.fromList
      . fmap ((\func -> (func ^. #name, func)) . fst)
      $ allFuncsSansPlts

-- | Makes a store of call nodes contained within a function
getCallNodesMapping
  :: [(Function, PilCfg)]
  -> HashMap Function [Cfg.CallNode [Pil.Stmt]]
getCallNodesMapping = HashMap.fromList . fmap (over _2 getCallNodes)
  where
    getCallNodes :: PilCfg -> [Cfg.CallNode [Pil.Stmt]]
    getCallNodes = mapMaybe (^? #_Call) . HashSet.toList . G.nodes

getConcreteFuncCallDest :: Cfg.CallNode [Pil.Stmt] -> Maybe Function
getConcreteFuncCallDest = (^? #callDest . #_CallFunc)

getExternalFuncCallDest :: Cfg.CallNode [Pil.Stmt] -> Maybe Pil.ConstFuncPtrOp
getExternalFuncCallDest cnode = do
  case cnode ^. #callDest of
    Pil.CallAddr x -> Just x
    Pil.CallExtern x -> Just $ Pil.ConstFuncPtrOp (x ^. #address) (x ^. #symbol)
    _ -> Nothing


-- | Replaces any calls to PLT thunks in Cfgs with direct calls.
-- Renames PLT thunks in function list (prepends `_`).
purgePltThunks :: [(Function, PilCfg)] -> [(Function, PilCfg)]
purgePltThunks allFuncCfgs = replaceCallSitesAndRenameThunks <$> allFuncCfgs
  where
    replaceCallSitesAndRenameThunks :: (Function, PilCfg) -> (Function, PilCfg)
    replaceCallSitesAndRenameThunks (func, cfg) = (func', cfg')
      where
        func' = maybe func (view #renamedFunc) . HashMap.lookup func $ pltMap
        cfg' = Cfg.safeMap replacePltThunkCallNode cfg

    replacePltThunkCallNode :: Cfg.PilNode -> Cfg.PilNode
    replacePltThunkCallNode = \case
      Cfg.Call x -> Cfg.Call $ case x ^. #callDest of
        Pil.CallFunc destFunc -> case HashMap.lookup destFunc pltMap of
          Nothing -> x
          Just thunkMapping -> x
            & #callDest .~ (thunkMapping ^. #newCallDest)
            & #nodeData %~ updateNodeData (thunkMapping ^. #newCallDest)
        _ ->  x
      n -> n

    updateNodeData :: Pil.CallDest Pil.Expression -> [Pil.Stmt] -> [Pil.Stmt]
    updateNodeData newDest = fmap (updateCallStatement newDest)

    updateCallStatement :: Pil.CallDest Pil.Expression -> Pil.Stmt -> Pil.Stmt
    updateCallStatement newDest stmt@(Pil.Stmt addr statement) = case statement of
      Pil.Call callOp -> Pil.Stmt addr . Pil.Call $ callOp & #dest .~ newDest
      Pil.TailCall tailCallOp -> Pil.Stmt addr . Pil.TailCall $ tailCallOp & #dest .~ newDest
      _ -> updateCallExpr newDest <$> stmt

    updateCallExpr
      :: Pil.CallDest Pil.Expression
      -> Pil.Expression
      -> Pil.Expression
    updateCallExpr newDest x = case x ^. #op of
      Pil.CALL callOp -> x & #op .~ Pil.CALL (callOp & #dest .~ newDest)
      _ -> x & #op %~ fmap (updateCallExpr newDest)


    pltMap :: HashMap Function PltThunkMapping
    pltMap = getPltThunkMapping allFuncCfgs


-- | Replaces any calls to interally pointing PLT thunks in Cfgs with direct calls.
-- Renames PLT thunks in function list (prepends `_`).
purgeInternalPltThunks :: [(Function, PilCfg)] -> [(Function, PilCfg)]
purgeInternalPltThunks allFuncCfgs = replaceCallSitesAndRenameThunks <$> allFuncCfgs
  where
    replaceCallSitesAndRenameThunks :: (Function, PilCfg) -> (Function, PilCfg)
    replaceCallSitesAndRenameThunks (func, cfg) = (func', cfg')
      where
        func' = case HashMap.lookup func pltMap of
          Nothing -> func
          Just x -> case isInternal x of
            False -> func
            True -> x ^. #renamedFunc
        cfg' = Cfg.safeMap replacePltThunkCallNode cfg

    isInternal :: PltThunkMapping -> Bool
    isInternal x = case x ^. #newCallDest of
      Pil.CallFunc _ -> True
      _ -> False

    replacePltThunkCallNode :: Cfg.PilNode -> Cfg.PilNode
    replacePltThunkCallNode = \case
      Cfg.Call x -> Cfg.Call $ case x ^. #callDest of
        Pil.CallFunc destFunc -> case HashMap.lookup destFunc pltMap of
          Nothing -> x
          Just thunkMapping -> case isInternal thunkMapping of
            True -> x
              & #callDest .~ (thunkMapping ^. #newCallDest)
              & #nodeData %~ updateNodeData (thunkMapping ^. #newCallDest)
            False -> x -- if it's not an internal func just leave it be
        _ ->  x
      n -> n

    updateNodeData :: Pil.CallDest Pil.Expression -> [Pil.Stmt] -> [Pil.Stmt]
    updateNodeData newDest = fmap (updateCallStatement newDest)

    updateCallStatement :: Pil.CallDest Pil.Expression -> Pil.Stmt -> Pil.Stmt
    updateCallStatement newDest stmt@(Pil.Stmt addr statement) = case statement of
      Pil.Call callOp -> Pil.Stmt addr . Pil.Call $ callOp & #dest .~ newDest
      Pil.TailCall tailCallOp -> Pil.Stmt addr . Pil.TailCall $ tailCallOp & #dest .~ newDest
      _ -> updateCallExpr newDest <$> stmt

    updateCallExpr
      :: Pil.CallDest Pil.Expression
      -> Pil.Expression
      -> Pil.Expression
    updateCallExpr newDest x = case x ^. #op of
      Pil.CALL callOp -> x & #op .~ Pil.CALL (callOp & #dest .~ newDest)
      _ -> x & #op %~ fmap (updateCallExpr newDest)


    pltMap :: HashMap Function PltThunkMapping
    pltMap = getPltThunkMapping allFuncCfgs

calcCallGraphFromInternalCallDests :: HashMap Function (HashSet Function) -> CallGraph
calcCallGraphFromInternalCallDests = G.fromEdges
  . concatMap getEdges
  . HashMap.toList
  where
    getEdges :: (Function, HashSet Function) -> [G.LEdge () Function]
    getEdges (bfunc, s) = do
      dest <- HashSet.toList s
      return . G.LEdge () $ G.Edge bfunc dest


getNewUuid :: UUID -> StateT (HashMap UUID UUID) IO UUID
getNewUuid old = do
  m <- get
  case HashMap.lookup old m of
    Just new -> return new
    Nothing -> do
      new <- lift randomIO
      modify $ HashMap.insert old new
      return new

traverseDescendantsMap
  :: forall m a b.
     ( Monad m
     , Hashable a
     , Hashable b
     )
  => (a -> m b)
  -> DescendantsMap a
  -> StateT (HashMap a b) m (DescendantsMap b)
traverseDescendantsMap f (DescendantsMap dmap) =
  DescendantsMap . HashMap.fromList <$> traverse g (HashMap.toList dmap)
  where
    g :: (a, HashSet a) -> StateT (HashMap a b) m (b, HashSet b)
    g (x, s) = do
      x' <- getMemoized f x
      s' <- fmap HashSet.fromList . traverse (getMemoized f) . HashSet.toList $ s
      return (x', s')

-- TODO: Reduce redundancy with above
traverseStrictDescendantsMap
  :: forall m a b.
     ( Monad m
     , Hashable a
     , Hashable b
     )
  => (a -> m b)
  -> StrictDescendantsMap a
  -> StateT (HashMap a b) m (StrictDescendantsMap b)
traverseStrictDescendantsMap f (StrictDescendantsMap dmap) =
  StrictDescendantsMap . HashMap.fromList <$> traverse g (HashMap.toList dmap)
  where
    g :: (a, HashSet a) -> StateT (HashMap a b) m (b, HashSet b)
    g (x, s) = do
      x' <- getMemoized f x
      s' <- fmap HashSet.fromList . traverse (getMemoized f) . HashSet.toList $ s
      return (x', s')

-- | Gets the CfgInfo, but all the UUIDs are fresh and new
getFreshFuncCfgInfo :: CfgStore -> Function -> IO (Maybe CfgInfo)
getFreshFuncCfgInfo store func = getFuncCfgInfo store func >>= \case
  Nothing -> return Nothing
  Just cfgInfo -> flip evalStateT HashMap.empty $ do
    cfg' <- Cfg.safeTraverse_ updateNodeId $ cfgInfo ^. #cfg
    acyclicCfg' <- Cfg.safeTraverse_ updateNodeId $ cfgInfo ^. #acyclicCfg
    dmap' <- traverseDescendantsMap updateNodeId $ cfgInfo ^. #descendantsMap
    sdmap' <- traverseStrictDescendantsMap updateNodeId $ cfgInfo ^. #strictDescendantsMap
    nodes' <- fmap HashSet.fromList
              . traverse (getMemoized updateNodeId)
              . HashSet.toList
              $ cfgInfo ^. #nodes
    calls' <- fmap (mapMaybe (^? #_Call))
              . traverse (getMemoized updateNodeId . Cfg.Call)
              $ cfgInfo ^. #calls
    return . Just $ CfgInfo
      { cfg = cfg'
      , acyclicCfg = acyclicCfg'
      , descendantsMap = dmap'
      , strictDescendantsMap = sdmap'
      , calls = calls'
      , nodes = nodes'
      }
      where
        updateNodeId :: PilNode -> IO PilNode
        updateNodeId n = do
          new <- randomIO
          return $ Cfg.setNodeUUID new n

-- | Gets the stored Cfg for a function, if it exists in the store.
getFuncCfgInfo :: CfgStore -> Function -> IO (Maybe CfgInfo)
getFuncCfgInfo store func = join <$> CC.get func (store ^. #cfgCache)

getAncestors :: CfgStore -> Function -> IO (Maybe (HashSet Function))
getAncestors store func = CC.get func $ store ^. #ancestorsCache

getFuncCfg :: CfgStore -> Function -> IO (Maybe PilCfg)
getFuncCfg store func = fmap (view #cfg) <$> getFuncCfgInfo store func

getCallGraph :: CfgStore -> IO CallGraph
getCallGraph store = fromJust <$> CC.get () (store ^. #callGraphCache)

getFuncs :: CfgStore -> IO [Function]
getFuncs store = fromJust <$> CC.get () (store ^. #funcs)

getTransposedCallGraph :: CfgStore -> IO CallGraph
getTransposedCallGraph store = fromJust <$> CC.get () (store ^. #transposedCallGraphCache)

-- | Adds a func/cfg to the store.
-- Overwrites existing function Cfg.
-- Any Cfgs in the store should have a CtxId of 0
addFunc'
  :: CfgStore -> Function -> Maybe PilCfg -> IO ()
addFunc' store func mcfg = CC.setCalc func (store ^. #cfgCache) $ do
  flip catch handleException . evaluate . fmap calcCfgInfo $ mcfg
  where
    handleException :: SomeException -> IO (Maybe CfgInfo)
    handleException e = do
      putText $ "\n---------- ERROR in Store: adding CfgInfo failed for " <> func ^. #name <> " ------------"
      print e
      return Nothing


-- | Adds a func/cfg to the store.
-- Overwrites existing function Cfg.
-- Any Cfgs in the store should have a CtxId of 0
addFunc
  :: ( CfgImporter a
     , NodeDataType a ~ PilNode)
  => a -> CfgStore -> Function -> IO ()
addFunc imp store func = CC.setCalc func (store ^. #cfgCache) $ do
  cfg <- ImpCfg.getCfg imp func 0
  flip catch handleException . evaluate . fmap (calcCfgInfo . view #result) $ cfg
  where
    handleException :: SomeException -> IO (Maybe CfgInfo)
    handleException e = do
      putText $ "\n---------- ERROR in Store: adding CfgInfo failed for " <> func ^. #name <> " ------------"
      print e
      return Nothing

-- | Adds a func/cfg to the store.
-- Overwrites existing function Cfg.
-- Any Cfgs in the store should have a CtxId of 0
calcCfgInfo :: PilCfg -> CfgInfo
calcCfgInfo cfg =
  CfgInfo
    { cfg = cfg
    , acyclicCfg = cfgWithoutBackedges
    , descendantsMap = dmap
    , strictDescendantsMap = strictDmap
    , nodes = G.nodes cfg
    , calls = calls
    }
  where
    cfgWithoutBackedges = makeCfgAcyclic cfg
    dmap = calcDescendantsMap cfgWithoutBackedges
    strictDmap = calcStrictDescendantsMap cfg
    calls = mapMaybe (^? #_Call) . HashSet.toList . G.nodes $ cfgWithoutBackedges

-- | This is pretty expensive and basically forces calculation of almost everything
-- in CfgStore.
getRouteMakerCtx'
  :: Word64
  -> CfgStore
  -> IO ( RouteMakerCtx Function PilNode
        , HashMap Function (HashSet PilNode)
        , HashSet PilNode
        )
getRouteMakerCtx' maxCallSearchDepth store = do
  cfgInfos <- fmap catHashMapMaybes . CC.getSnapshot $ store ^. #cfgCache
  let funcInnerNodes = view #nodes <$> cfgInfos
      startNodes = Cfg.getRootNode . view #cfg <$> cfgInfos
      dmaps = view #strictDescendantsMap <$> cfgInfos
  callAncestors <- CC.getSnapshot $ store ^. #ancestorsCache
  let outerNodeDescendants = G.calcOuterNodeDescendants (fromMaybe HashSet.empty . flip HashMap.lookup funcInnerNodes) callAncestors

      ctx = RouteMakerCtx
        { getTransNodeContext = getCallTargetFunc
        , getStartNode = startNodes
        , getDescendantsMap = dmaps
        , outerContextNodeDescendants = outerNodeDescendants
        , maxCallDepth = maxCallSearchDepth
        }

      allNodes = HashSet.unions . HashMap.elems $ funcInnerNodes
        
  return (ctx, funcInnerNodes, allNodes)

  where
    getCallTargetFunc :: PilNode -> Maybe Function
    getCallTargetFunc (Cfg.Call n) = CfgI.getTargetFunc n
    getCallTargetFunc _ = Nothing

getFuncNameMapping :: CfgStore -> IO (HashMap Text Function)
getFuncNameMapping
  = fmap (foldl'
          (\m func -> HashMap.insert (func ^. #name) func m)
          HashMap.empty
         )
  . CC.getKeys
  . view #cfgCache

-- | Replaces the CfgInfo of the old func with that of the new func.
-- This can be used to shimmy in cfgs from other binaries/libraries.
-- Any functions used in call nodes in the new Cfg that have the same name
-- as functions in the CfgStore will be replaced by the funcs in the CfgStore.
-- IMPORTANT: Shimmy in functions before calculating things like the RouteMakerCtx
-- TODO: figure out how to deal with instruction addresses inside new func cfg.
--       i.e. they might be in an overlapping range between the two binaries
-- TODO: handle case where you want to shimmy funcs that might not be in store yet.
--       New funcs are added for each call a shimmied func makes.
shimmyFunc
  :: ( CfgImporter a
     , NodeDataType a ~ PilNode)
  => a
  -> CfgStore
  -> Function -- Func1, whose CfgInfo to replace
  -> Function -- Func2, whose CfgInfo will replace func1
  -> IO ()
shimmyFunc imp store func1 func2 = do
  -- We don't calculate this lazily because the inner calls need to immediately update
  -- the call graph and the functions list.
  fmap (view #result) <$> ImpCfg.getCfg imp func2 0 >>= \case
    Nothing -> return ()
    Just cfg -> do
      funcNameMapping <- getFuncNameMapping store
      let callNodes = mapMaybe (^? #_Call) . HashSet.toList . G.nodes $ cfg
          (funcDests, callNodeSubsts, newFuncs) = foldl' (f funcNameMapping) (HashSet.empty, [], HashSet.empty) callNodes
          cfg' = foldl' (\g (a, b) -> G.updateNode (const $ Cfg.Call b) (Cfg.Call a) g) cfg callNodeSubsts
      -- TODO: maybe add newFuncs to store ^. #funcs (which right now is immutable)

      -- Add new call dests for func1
      -- TODO: remove old call dests?
      CC.modifyCalc (addNewFuncDests funcDests) () (store ^. #callGraphCache)

      -- Reset transposed call graph to be based on current call graph
      -- TODO: this is inefficent if you are shimming multiple funcs
      CC.setCalc () (store ^. #transposedCallGraphCache) $ do
        cg <- getCallGraph store
        return $ G.transpose cg

      -- Set CfgInfo for func1 to new Cfg
      CC.setCalc func1 (store ^. #cfgCache) . return . Just . calcCfgInfo $ cfg'

      -- Add Nothing cfg info for new funcs, just in case they get looked up
      forM_ (HashSet.toList newFuncs) $ \newFunc -> do
        CC.setCalc newFunc (store ^. #cfgCache) $ return Nothing        
  where    
    addNewFuncDests :: HashSet Function -> Maybe CallGraph -> IO CallGraph
    addNewFuncDests _ Nothing = error "call graph must already be loaded"
    addNewFuncDests dests (Just cg) = do
      return
        . foldl' (flip G.addEdge) cg
        . fmap (G.LEdge () . G.Edge func1)
        . HashSet.toList
        $ dests

    f :: HashMap Text Function
      -> ( HashSet Function
         , [(Cfg.CallNode a, Cfg.CallNode a)]
         , HashSet Function
         )
      -> Cfg.CallNode a
      -> ( HashSet Function
         , [(Cfg.CallNode a, Cfg.CallNode a)]
         , HashSet Function
         )
    f funcNameMapping asIs@(destFuncs, callNodeSubsts, newFuncs) n = case CfgI.getCallTargetFunction $ n ^. #callDest of
      Nothing -> asIs
      Just func -> case HashMap.lookup (func ^. #name) funcNameMapping of
        -- No function with this name exists in store
        Nothing -> (HashSet.insert func destFuncs, callNodeSubsts, HashSet.insert func newFuncs)
        Just existingFunc ->
          ( HashSet.insert existingFunc destFuncs
          , (n, n & #callDest .~ Pil.CallFunc existingFunc) : callNodeSubsts
          , newFuncs
          )
        
shimmyFuncByName
  :: ( CfgImporter a
     , NodeDataType a ~ PilNode)
  => a
  -> CfgStore
  -> Text -- Name of Func1, whose CfgInfo to replace
  -> Function -- Func2, whose CfgInfo will replace func1
  -> IO ()
shimmyFuncByName imp store func1name func2 = do
  m <- getFuncNameMapping store
  case HashMap.lookup func1name m of
    Nothing -> error $ "Couldn't find func " <> cs func1name <> " in store"
    Just func1 -> shimmyFunc imp store func1 func2

-- -- | This finds PTL thunks replaces their Cfg with a call to the func with
-- -- the corresponding name.
-- -- It also updates the call graph.
-- linkPltThunks :: CfgStore -> IO ()
-- linkPltThunks store
