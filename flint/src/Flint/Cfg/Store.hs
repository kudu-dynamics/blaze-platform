{- HLINT ignore "Redundant <$>" -}

module Flint.Cfg.Store where

import Flint.Prelude

import Flint.Analysis.Path.Matcher.Primitives (getInitialWMIs)
import Flint.Types.Analysis.Path.Matcher.Primitives (KnownFunc)
import Flint.Types.Cfg.Store
import Flint.Util (offsetUUID, timeIt, timingLog, samplingTimingEnabled)
import Data.IORef (readIORef)
import qualified Flint.Types.CachedCalc as CC
import qualified Flint.Types.CachedMap as CM

import qualified Blaze.CallGraph as CG
import qualified Blaze.Import.Source.Ghidra.CallGraph as GhidraCG
import qualified Blaze.Cfg.Interprocedural as CfgI
import Blaze.Cfg.Path (makeCfgAcyclic)
import Blaze.Graph (RouteMakerCtx(RouteMakerCtx))
import qualified Blaze.Graph as G
import qualified Blaze.Persist.Db as Db
import Blaze.Types.Function (Function, Func)
import qualified Blaze.Types.Function as Func
import Blaze.Types.Graph (calcStrictDescendantsMap, StrictDescendantsMap(StrictDescendantsMap))
import qualified Blaze.Types.Pil as Pil

import Blaze.Import.Binary (BinaryImporter)
import qualified Blaze.Import.Binary as Binary
import qualified Blaze.Import.CallGraph as CG
import Blaze.Import.CallGraph (CallGraphImporter)
import qualified Blaze.Import.Cfg as ImpCfg
import Blaze.Import.Cfg (CfgImporter, NodeDataType)
import Blaze.Import.Xref (XrefImporter)
import qualified Blaze.Import.Xref as Xref

import Blaze.Types.CallGraph (CallGraph, CallSite)
import Blaze.Types.Cfg (PilCfg, PilNode)
import qualified Blaze.Types.Cfg as Cfg
import Blaze.Util (getMemoized)
import Blaze.Types.Import (TypeHints)


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
     , BinaryImporter imp
     , CfgImporter imp
     , XrefImporter imp
     , Typeable imp
     )
  => Maybe FilePath -> imp -> IO CfgStore
init mDbFilePath imp = do
  (cfgStore, _) <- initWithTypeHints HashSet.empty HashSet.empty mDbFilePath imp
  return cfgStore


initWithTypeHints
  :: ( CallGraphImporter imp
     , NodeDataType imp ~ PilNode
     , BinaryImporter imp
     , CfgImporter imp
     , XrefImporter imp
     , Typeable imp
     )
  => HashSet Text     -- whitelist for functions we want to have type hints for
  -> HashSet Text     -- blacklist of function names to exclude from analysis
  -> Maybe FilePath
  -> imp
  -> IO (CfgStore, HashMap Function TypeHints)
initWithTypeHints typeHintsWhitelist blacklist mDbFilePath imp = do
  mDb <- case mDbFilePath of
    Nothing -> return Nothing
    Just fp -> Just <$> Db.init fp

  debug "  [store] Getting strings..."
  smap <- Binary.getStringsMap imp
  debug $ "  [store] Found " <> show (HashMap.size smap) <> " strings."

  debug "  [store] Getting string xrefs..."
  let strAddrs = HashMap.keys smap
      totalStrs = length strAddrs
  sxrefs <- fmap HashMap.fromList . forM (zip [(1::Int)..] strAddrs) $ \(i, addr) -> do
    debug $ "  [store] xrefs " <> show i <> "/" <> show totalStrs
    xrefs <- Xref.getXrefsTo imp addr
    return (addr, xrefs)
  debug $ "  [store] Got " <> show (HashMap.size sxrefs) <> " xrefs."

  debug "  [store] Creating caches..."
  store <- CfgStore
    <$> atomically CC.create
    <*> atomically CC.create -- acyclicCfgCache
    <*> atomically CC.create -- acyclicDescendantsCache
    <*> atomically CC.create
    <*> atomically CC.create
    <*> atomically CC.create
    <*> atomically CC.create
    <*> atomically CC.create
    <*> atomically CC.create
    <*> atomically CC.create -- callSitesInFuncCache
    <*> atomically CC.create -- callSitesToFuncCache
    <*> atomically (CM.create [])
    <*> atomically (CM.create HashSet.empty)
    <*> Binary.getBase imp
    <*> pure smap
    <*> pure sxrefs

  debug "  [store] Getting functions..."
  allFuncs <- CG.getFunctions imp

  let internalFuncs :: [Function]
      internalFuncs = filter (\f -> not $ HashSet.member (f ^. #name) blacklist)
                    $ mapMaybe (^? #_Internal) allFuncs
  debug $ "  [store] Got " <> show (length allFuncs) <> " functions (" <> show (length internalFuncs) <> " internal)."

  -- Only eagerly fetch CFGs for functions in the type hints whitelist.
  -- All other CFGs are computed lazily on demand.
  (eagerCfgMap, funcToTypeHints) <- if HashSet.null typeHintsWhitelist
    then return (HashMap.empty, [])
    else do
      (cfgs, hints) <- getFuncsWithCfgsAndTypeHints imp typeHintsWhitelist
                        (filter (\f -> HashSet.member (f ^. #name) typeHintsWhitelist) internalFuncs)
      return (HashMap.fromList cfgs, hints)

  let functionToTypeHintsMap :: HashMap Function TypeHints
      functionToTypeHintsMap = HashMap.fromList funcToTypeHints

  CC.setCalc () (store ^. #funcs) $ return allFuncs
  CC.setCalc () (store ^. #internalFuncs) $ return internalFuncs
  CC.setCalc () (store ^. #callGraphCache) $ do
    -- Use cached call graph builder for GhidraImporter (avoids redundant
    -- JNI calls when the same function appears as caller in many references).
    let getCG = case cast imp of
          Just ghidraImp -> GhidraCG.getCallGraphCached ghidraImp allFuncs
          Nothing -> CG.getCallGraph imp allFuncs
    (cg, cgTime) <- timeIt $ case mDb of
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
    timingLog $ "[timing] getCallGraph (" <> show (length allFuncs) <> " funcs): " <> show cgTime
    return cg
  CC.setCalc () (store ^. #transposedCallGraphCache) $ do
    cg <- getCallGraph store
    return $ G.transpose cg
  -- Set up calcs for ancestors and call sites to each function
  let totalFuncs = length allFuncs
      totalInternal = length internalFuncs
  debug "  [store] Setting up function caches..."
  forM_ (zip [(1::Int)..] allFuncs) $ \(i, func) -> do
    debug $ "  [store] func caches " <> show i <> "/" <> show totalFuncs
    CC.setCalc func (store ^. #ancestorsCache) $ do
      cg <- fromJust <$> CC.get () (store ^. #transposedCallGraphCache)
      return $ G.getStrictDescendants func cg
    CC.setCalc func (store ^. #descendantsCache) $ do
      cg <- fromJust <$> CC.get () (store ^. #callGraphCache)
      return $ G.getStrictDescendants func cg
    CC.setCalc func (store ^. #callSitesToFuncCache) $
      CG.getCallSites imp func
  debug "  [store] Setting up CFG caches..."
  forM_ (zip [(1::Int)..] internalFuncs) $ \(i, func) -> do
    debug $ "  [store] CFG caches " <> show i <> "/" <> show totalInternal
    CC.setCalc func (store ^. #callSitesInFuncCache) $ do
      -- Compute call sites lazily from the CFG
      getFuncCfgInfo store func >>= \case
        Nothing -> return []
        Just cfgInfo -> return $ mapMaybe toCallSite (cfgInfo ^. #calls)
          where
            toCallSite n = case n ^. #callDest of
                Pil.CallFunc destFunc -> Just
                  $ CG.CallSite func (n ^. #start) (Func.Internal destFunc)
                Pil.CallExtern destExtern -> Just
                  $ CG.CallSite func (n ^. #start) (Func.External destExtern)
                _ -> Nothing
    -- Use pre-computed CFG for whitelisted functions, lazy for the rest
    case HashMap.lookup func eagerCfgMap of
      Just cfg -> addFunc' store func (Just cfg)
      Nothing  -> addFunc imp store func
    -- Set up lazy acyclic CFG computation
    CC.setCalc func (store ^. #acyclicCfgCache) $
      getFuncCfgInfo store func >>= \case
        Nothing -> return Nothing
        Just cfgInfo -> return . Just . makeCfgAcyclic $ cfgInfo ^. #cfg

    -- Set up lazy acyclic descendants map computation
    CC.setCalc func (store ^. #acyclicDescendantsCache) $
      getAcyclicCfg store func >>= \case
        Nothing -> return Nothing
        Just acfg -> return . Just . calcStrictDescendantsMap $ acfg

  -- When profiling is enabled, force call graph computation eagerly so it's timed
  whenM (readIORef samplingTimingEnabled) $
    void $ getCallGraph store

  return (store, functionToTypeHintsMap)

-- | Returns a list of all the functions in the binary that we can make a Cfg for.
getFuncsWithCfgs
  :: ( CfgImporter a
     , NodeDataType a ~ PilNode
     )
  => a
  -> [Function]
  -> IO [(Function, PilCfg)]
getFuncsWithCfgs imp funcs = do
  fmap catMaybes . forM funcs $ \func -> fmap (func,) <$> ImpCfg.getCfg_ imp func 0

-- | Returns a tuple that contains the functions in the binary that we can make a Cfg for and functions that we want type hints for
getFuncsWithCfgsAndTypeHints
  :: ( CfgImporter a
     , NodeDataType a ~ PilNode
     )
  => a
  -> HashSet Text     -- function whitelist for typehints
  -> [Function]
  -> IO ([(Function, PilCfg)], [(Function,TypeHints)])
getFuncsWithCfgsAndTypeHints imp whitelist = foldM updateTuple ([],[])
  where
    updateTuple :: ([(Function,PilCfg)], [(Function, TypeHints)]) -> Function -> IO ([(Function,PilCfg)], [(Function, TypeHints)])
    updateTuple (funcToCfg, funcToTypeHints) func = do
      (cfg, typeHints) <- ImpCfg.getCfgAndTypeHints_ imp func 0
      let funcToCfg' = maybe funcToCfg (\cfg' -> (func,cfg'):funcToCfg) cfg
      let funcToTypeHints' = if HashSet.member (func ^. #name) whitelist && (not . null) typeHints then (func,typeHints):funcToTypeHints else funcToTypeHints
      return (funcToCfg', funcToTypeHints')

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
         ( [ Pil.Def (Pil.DefOp v (Pil.Expression _ (Pil.CALL (Pil.CallOp dest@(Pil.CallAddr (Pil.ConstFuncPtrOp _ (Just x))) _)))) ]
           , [[ Pil.Ret (Pil.RetOp (Pil.Expression _ (Pil.VAR (Pil.VarOp v')))) ]]) ->
           if v == v' && x == srcFunc ^. #name
           then Just dest
           else Nothing
         ( [ Pil.Def (Pil.DefOp v (Pil.Expression _ (Pil.CALL (Pil.CallOp dest@(Pil.CallFunc destFunc) _)))) ]
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

getNewUuid :: UUID -> StateT (HashMap UUID UUID) IO UUID
getNewUuid old = do
  m <- get
  case HashMap.lookup old m of
    Just new -> return new
    Nothing -> do
      new <- lift randomIO
      modify $ HashMap.insert old new
      return new

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

-- | Gets the CfgInfo, but all the UUIDs are fresh random ones.
--   Prefer getOffsetFuncCfgInfo for deterministic UUID remapping.
getFreshFuncCfgInfo :: CfgStore -> Function -> IO (Maybe CfgInfo)
getFreshFuncCfgInfo store func = getFuncCfgInfo store func >>= \case
  Nothing -> return Nothing
  Just cfgInfo -> flip evalStateT HashMap.empty $ do
    cfg' <- Cfg.safeTraverse_ randomNode $ cfgInfo ^. #cfg
    sdmap' <- traverseStrictDescendantsMap randomNode $ cfgInfo ^. #strictDescendantsMap
    nodes' <- fmap HashSet.fromList
              . traverse (getMemoized randomNode)
              . HashSet.toList
              $ cfgInfo ^. #nodes
    calls' <- fmap (mapMaybe (^? #_Call))
              . traverse (getMemoized randomNode . Cfg.Call)
              $ cfgInfo ^. #calls
    return . Just $ CfgInfo
      { cfg = cfg'
      , strictDescendantsMap = sdmap'
      , calls = calls'
      , nodes = nodes'
      }

-- | Gets the CfgInfo with UUIDs deterministically offset by n.
--   Same function + same offset always produces the same UUIDs,
--   which preserves nub's ability to dedup identical paths.
getOffsetFuncCfgInfo :: CfgStore -> Function -> Word64 -> IO (Maybe CfgInfo)
getOffsetFuncCfgInfo store func n = getFuncCfgInfo store func >>= \case
  Nothing -> return Nothing
  Just cfgInfo -> flip evalStateT HashMap.empty $ do
    cfg' <- Cfg.safeTraverse_ (offsetNode n) $ cfgInfo ^. #cfg
    sdmap' <- traverseStrictDescendantsMap (offsetNode n) $ cfgInfo ^. #strictDescendantsMap
    nodes' <- fmap HashSet.fromList
              . traverse (getMemoized (offsetNode n))
              . HashSet.toList
              $ cfgInfo ^. #nodes
    calls' <- fmap (mapMaybe (^? #_Call))
              . traverse (getMemoized (offsetNode n) . Cfg.Call)
              $ cfgInfo ^. #calls
    return . Just $ CfgInfo
      { cfg = cfg'
      , strictDescendantsMap = sdmap'
      , calls = calls'
      , nodes = nodes'
      }

-- | Gets the acyclic CFG for a function from the cache.
getAcyclicCfg :: CfgStore -> Function -> IO (Maybe PilCfg)
getAcyclicCfg store func = join <$> CC.get func (store ^. #acyclicCfgCache)

-- | Gets the strict descendants map computed from the acyclic CFG.
-- This is the correct map to use with LoopSampler, which doesn't follow back edges.
getAcyclicDescendantsMap :: CfgStore -> Function -> IO (Maybe (StrictDescendantsMap PilNode))
getAcyclicDescendantsMap store func = join <$> CC.get func (store ^. #acyclicDescendantsCache)

-- | Gets the acyclic CFG with fresh random UUIDs.
getFreshAcyclicCfg :: CfgStore -> Function -> IO (Maybe PilCfg)
getFreshAcyclicCfg store func = getAcyclicCfg store func >>= \case
  Nothing -> return Nothing
  Just acfg -> Just <$> Cfg.safeTraverse randomNode acfg

-- | Gets the acyclic CFG with UUIDs deterministically offset by n.
getOffsetAcyclicCfg :: CfgStore -> Function -> Word64 -> IO (Maybe PilCfg)
getOffsetAcyclicCfg store func n = getAcyclicCfg store func >>= \case
  Nothing -> return Nothing
  Just acfg -> Just <$> Cfg.safeTraverse (offsetNode n) acfg

offsetNode :: Applicative f => Word64 -> PilNode -> f PilNode
offsetNode n node = pure $ Cfg.setNodeUUID (offsetUUID n uuid) node
  where uuid = Cfg.getNodeUUID node

randomNode :: PilNode -> IO PilNode
randomNode n = do
  new <- randomIO
  return $ Cfg.setNodeUUID new n

-- | Gets the stored Cfg for a function, if it exists in the store.
getFuncCfgInfo :: CfgStore -> Function -> IO (Maybe CfgInfo)
getFuncCfgInfo store func = join <$> CC.get func (store ^. #cfgCache)

getAncestors :: CfgStore -> Func -> IO (Maybe (HashSet Func))
getAncestors store func = CC.get func $ store ^. #ancestorsCache

-- | Temporary convenience function for functions that still work with Function instead of Func
getAncestors' :: CfgStore -> Function -> IO (Maybe (HashSet Function))
getAncestors' store func = mapMaybeHashSet (^? #_Internal)
  <<$>> getAncestors store (Func.Internal func)


getDescendants :: CfgStore -> Func -> IO (Maybe (HashSet Func))
getDescendants store func = CC.get func $ store ^. #descendantsCache

-- | Temporary convenience function for functions that still work with Function instead of Func
getDescendants' :: CfgStore -> Function -> IO (Maybe (HashSet Function))
getDescendants' store func = mapMaybeHashSet (^? #_Internal)
  <<$>> getDescendants store (Func.Internal func)


getFuncCfg :: CfgStore -> Function -> IO (Maybe PilCfg)
getFuncCfg store func = fmap (view #cfg) <$> getFuncCfgInfo store func

getCallGraph :: CfgStore -> IO CallGraph
getCallGraph store = fromJust <$> CC.get () (store ^. #callGraphCache)

getFuncs :: CfgStore -> IO [Func]
getFuncs store = fromJust <$> CC.get () (store ^. #funcs)

getInternalFuncs :: CfgStore -> IO [Function]
getInternalFuncs store = fromJust <$> CC.get () (store ^. #internalFuncs)

getExternalFuncs :: CfgStore -> IO [Func.ExternFunction]
getExternalFuncs store = mapMaybe (^? #_External) <$> getFuncs store

-- | Get call sites that target a given function (callee → callers)
getCallSitesToFunc :: CfgStore -> Func -> IO [CallSite]
getCallSitesToFunc store func = fromMaybe [] <$> CC.get func (store ^. #callSitesToFuncCache)

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
    , strictDescendantsMap = calcStrictDescendantsMap cfg
    , nodes = G.nodes cfg
    , calls = mapMaybe (^? #_Call) . HashSet.toList . G.nodes $ cfg
    }

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
  cfgInfos :: HashMap Function CfgInfo <- fmap catHashMapMaybes
                                          . CC.getSnapshot
                                          $ store ^. #cfgCache
  let funcInnerNodes = view #nodes <$> cfgInfos
      startNodes = Cfg.getRootNode . view #cfg <$> cfgInfos
      dmaps = view #strictDescendantsMap <$> cfgInfos
  callAncestors <- fmap toFunctionAncestorMap . CC.getSnapshot $ store ^. #ancestorsCache
  
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
    toFunction = (^? #_Internal)
    toFunctionAncestorMap :: HashMap Func (HashSet Func) -> HashMap Function (HashSet Function)
    toFunctionAncestorMap = HashMap.fromList
      . mapMaybe (\(func, s) -> func ^? #_Internal >>= \function ->
                     return ( function
                            , HashSet.fromList
                              . mapMaybe toFunction
                              . HashSet.toList
                              $ s))
      . HashMap.toList
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


-- TODO: get shiffyFunc to work with new Extern/internal Func type

-- -- | Replaces the CfgInfo of the old func with that of the new func.
-- -- This can be used to shimmy in cfgs from other binaries/libraries.
-- -- Any functions used in call nodes in the new Cfg that have the same name
-- -- as functions in the CfgStore will be replaced by the funcs in the CfgStore.
-- -- IMPORTANT: Shimmy in functions before calculating things like the RouteMakerCtx
-- -- TODO: figure out how to deal with instruction addresses inside new func cfg.
-- --       i.e. they might be in an overlapping range between the two binaries
-- -- TODO: handle case where you want to shimmy funcs that might not be in store yet.
-- --       New funcs are added for each call a shimmied func makes.
-- shimmyFunc
--   :: ( CfgImporter a
--      , NodeDataType a ~ PilNode)
--   => a
--   -> CfgStore
--   -> Function -- Func1, whose CfgInfo to replace
--   -> Function -- Func2, whose CfgInfo will replace func1
--   -> IO ()
-- shimmyFunc imp store func1 func2 = do
--   -- We don't calculate this lazily because the inner calls need to immediately update
--   -- the call graph and the functions list.
--   fmap (view #result) <$> ImpCfg.getCfg imp func2 0 >>= \case
--     Nothing -> return ()
--     Just cfg -> do
--       funcNameMapping <- getFuncNameMapping store
--       let callNodes = mapMaybe (^? #_Call) . HashSet.toList . G.nodes $ cfg
--           (funcDests, callNodeSubsts, newFuncs) = foldl' (f funcNameMapping) (HashSet.empty, [], HashSet.empty) callNodes
--           cfg' = foldl' (\g (a, b) -> G.updateNode (const $ Cfg.Call b) (Cfg.Call a) g) cfg callNodeSubsts
--       -- TODO: maybe add newFuncs to store ^. #funcs (which right now is immutable)

--       -- Add new call dests for func1
--       -- TODO: remove old call dests?
--       CC.modifyCalc (addNewFuncDests funcDests) () (store ^. #callGraphCache)

--       -- Reset transposed call graph to be based on current call graph
--       -- TODO: this is inefficent if you are shimming multiple funcs
--       CC.setCalc () (store ^. #transposedCallGraphCache) $ do
--         cg <- getCallGraph store
--         return $ G.transpose cg

--       -- Set CfgInfo for func1 to new Cfg
--       CC.setCalc func1 (store ^. #cfgCache) . return . Just . calcCfgInfo $ cfg'

--       -- Add Nothing cfg info for new funcs, just in case they get looked up
--       forM_ (HashSet.toList newFuncs) $ \newFunc -> do
--         CC.setCalc newFunc (store ^. #cfgCache) $ return Nothing        
--   where    
--     addNewFuncDests :: HashSet Function -> Maybe CallGraph -> IO CallGraph
--     addNewFuncDests _ Nothing = error "call graph must already be loaded"
--     addNewFuncDests dests (Just cg) = do
--       return
--         . foldl' (flip G.addEdge) cg
--         . fmap (G.LEdge () . G.Edge func1)
--         . HashSet.toList
--         $ dests

--     f :: HashMap Text Function
--       -> ( HashSet Function
--          , [(Cfg.CallNode a, Cfg.CallNode a)]
--          , HashSet Function
--          )
--       -> Cfg.CallNode a
--       -> ( HashSet Function
--          , [(Cfg.CallNode a, Cfg.CallNode a)]
--          , HashSet Function
--          )
--     f funcNameMapping asIs@(destFuncs, callNodeSubsts, newFuncs) n = case CfgI.getCallTargetFunction $ n ^. #callDest of
--       Nothing -> asIs
--       Just func -> case HashMap.lookup (func ^. #name) funcNameMapping of
--         -- No function with this name exists in store
--         Nothing -> (HashSet.insert func destFuncs, callNodeSubsts, HashSet.insert func newFuncs)
--         Just existingFunc ->
--           ( HashSet.insert existingFunc destFuncs
--           , (n, n & #callDest .~ Pil.CallFunc existingFunc) : callNodeSubsts
--           , newFuncs
--           )
        
-- shimmyFuncByName
--   :: ( CfgImporter a
--      , NodeDataType a ~ PilNode)
--   => a
--   -> CfgStore
--   -> Text -- Name of Func1, whose CfgInfo to replace
--   -> Function -- Func2, whose CfgInfo will replace func1
--   -> IO ()
-- shimmyFuncByName imp store func1name func2 = do
--   m <- getFuncNameMapping store
--   case HashMap.lookup func1name m of
--     Nothing -> error $ "Couldn't find func " <> cs func1name <> " in store"
--     Just func1 -> shimmyFunc imp store func1 func2


-- | Looks through all funcs to find which KnownFuncs are in play,
-- generates CallablePrimitives from these known funcs and stores it in Cfg,
-- overwriting whatever CallablePrimitives may already be there.
populateInitialPrimitives
  :: [KnownFunc]
  -> CfgStore
  -> IO ()
populateInitialPrimitives sprims store = do
  funcs <- getFuncs store
  CM.putSnapshot (getInitialWMIs sprims funcs) $ store ^. #callablePrims

-- | Add new KnownFuncs to the existing callable primitives (merge, not replace).
addKnownFuncs
  :: [KnownFunc]
  -> CfgStore
  -> IO ()
addKnownFuncs newFuncs store = do
  funcs <- getFuncs store
  existing <- CM.getSnapshot $ store ^. #callablePrims
  let newWMIs = getInitialWMIs newFuncs funcs
      merged = HashMap.unionWith HashSet.union existing newWMIs
  CM.putSnapshot merged $ store ^. #callablePrims

-- | Replace the set of known-function-derived primitives while preserving
-- any primitives learned dynamically during analysis.
replaceKnownFuncsPreservingLearned
  :: [KnownFunc]
  -> [KnownFunc]
  -> CfgStore
  -> IO ()
replaceKnownFuncsPreservingLearned oldFuncs newFuncs store = do
  funcs <- getFuncs store
  existing <- CM.getSnapshot $ store ^. #callablePrims
  let oldWMIs = getInitialWMIs oldFuncs funcs
      newWMIs = getInitialWMIs newFuncs funcs
      learnedOnly = HashMap.differenceWith dropInitial existing oldWMIs
      merged = HashMap.unionWith HashSet.union learnedOnly newWMIs
  CM.putSnapshot merged $ store ^. #callablePrims
  where
    dropInitial current initial =
      let remaining = HashSet.difference current initial
      in if HashSet.null remaining then Nothing else Just remaining
