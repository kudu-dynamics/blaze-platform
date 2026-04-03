{- HLINT ignore "Redundant <$>" -}

module Flint.Cfg.Store where

import Flint.Prelude

import Flint.Analysis.Path.Matcher.Primitives (getInitialWMIsFromStore)
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
import Blaze.Types.Function (Function, Func, FuncRef, FunctionRef, toFunctionRef, toExternFunctionRef)
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
    <$> atomically CC.create                  -- cfgCache
    <*> atomically CC.create                  -- acyclicCfgCache
    <*> atomically CC.create                  -- acyclicDescendantsCache
    <*> atomically CC.create                  -- ancestorsCache
    <*> atomically CC.create                  -- descendantsCache
    <*> atomically CC.create                  -- funcs
    <*> atomically CC.create                  -- internalFuncs
    <*> atomically CC.create                  -- funcCalc
    <*> atomically CC.create                  -- externFuncCalc
    <*> atomically CC.create                  -- callGraphCache
    <*> atomically CC.create                  -- transposedCallGraphCache
    <*> atomically CC.create                  -- callSitesInFuncCache
    <*> atomically CC.create                  -- callSitesToFuncCache
    <*> atomically (CM.create [])             -- pathSamples
    <*> atomically (CM.create HashSet.empty)  -- callablePrims
    <*> Binary.getBase imp                    -- baseOffset
    <*> pure smap                             -- stringsMap
    <*> pure sxrefs                           -- stringXrefs

  debug "  [store] Getting functions..."
  allFuncRefs <- CG.getFunctions imp

  let internalFuncRefs :: [FunctionRef]
      internalFuncRefs = filter (\fm -> not $ HashSet.member (fm ^. #name) blacklist)
                          $ mapMaybe (^? #_InternalRef) allFuncRefs
  debug $ "  [store] Got " <> show (length allFuncRefs) <> " functions (" <> show (length internalFuncRefs) <> " internal)."

  -- Type hints require full Functions with params (eager decompilation for whitelisted funcs only).
  (eagerCfgMap, funcToTypeHints) <- if HashSet.null typeHintsWhitelist
    then return (HashMap.empty, [])
    else do
      let whitelistedRefs = filter (\fm -> HashSet.member (fm ^. #name) typeHintsWhitelist) internalFuncRefs
      whitelistedFuncs <- forM whitelistedRefs $ \fm ->
        CG.getFunction imp (fm ^. #address) >>= \case
          Just (Func.Internal f) -> return f
          _ -> error $ "Could not resolve function at " <> show (fm ^. #address)
      (cfgs, hints) <- getFuncsWithCfgsAndTypeHints imp typeHintsWhitelist whitelistedFuncs
      return (HashMap.fromList cfgs, hints)

  let functionToTypeHintsMap :: HashMap Function TypeHints
      functionToTypeHintsMap = HashMap.fromList funcToTypeHints

  -- Set default computations for CFG-layer caches.
  -- These fire on-demand when `get` is called with an unknown Function key.
  let handleCfgException :: Text -> SomeException -> IO (Maybe CfgInfo)
      handleCfgException funcName e = do
        putText $ "\n---------- ERROR in Store: adding CfgInfo failed for " <> funcName <> " ------------"
        print e
        return Nothing
  CC.setDefault (\func -> do
    mcfg <- ImpCfg.getCfg_ imp func 0
    flip catch (handleCfgException $ func ^. #name) . evaluate . fmap calcCfgInfo $ mcfg
    ) (store ^. #cfgCache)
  CC.setDefault (getFuncCfgInfo store >=>
    (\case
      Nothing -> return Nothing
      Just cfgInfo -> return . Just . makeCfgAcyclic $ cfgInfo ^. #cfg)
    ) (store ^. #acyclicCfgCache)
  CC.setDefault (getAcyclicCfg store >=>
    (\case
      Nothing -> return Nothing
      Just acfg -> return . Just . calcStrictDescendantsMap $ acfg)
    ) (store ^. #acyclicDescendantsCache)
  CC.setDefault (\func ->
    getFuncCfgInfo store func >>= \case
      Nothing -> return []
      Just cfgInfo -> return $ mapMaybe (toCallSite func) (cfgInfo ^. #calls)
    ) (store ^. #callSitesInFuncCache)

  CC.setCalc () (store ^. #funcs) $ return allFuncRefs
  CC.setCalc () (store ^. #internalFuncs) $ return internalFuncRefs

  -- Register lazy decompilation for each internal function.
  forM_ internalFuncRefs $ \marker ->
    CC.setCalc marker (store ^. #funcCalc) $
      CG.getFunction imp (marker ^. #address) >>= \case
        Just (Func.Internal fullFunc) -> return fullFunc
        _ -> error $ "Could not resolve function at " <> show (marker ^. #address)

  -- Seed funcCalc with already-resolved whitelisted functions.
  forM_ funcToTypeHints $ \(func, _hints) ->
    CC.set (toFunctionRef func) func (store ^. #funcCalc)

  -- Register lazy extern resolution for each external function.
  -- Uses getFunction which handles EXTERNAL address space via mkExternalAddress.
  let externRefs = mapMaybe (^? #_ExternalRef) allFuncRefs
  forM_ externRefs $ \ref' ->
    CC.setCalc ref' (store ^. #externFuncCalc) $
      CG.getFunction imp (ref' ^. #address) >>= \case
        Just (Func.External ef) -> return ef
        _ -> error $ "Could not resolve extern function at " <> show (ref' ^. #address)

  -- Pre-populate cfgCache for whitelisted functions (already computed).
  forM_ (HashMap.toList eagerCfgMap) $ \(func, cfg) ->
    CC.set func (Just $ calcCfgInfo cfg) (store ^. #cfgCache)

  -- Call graph and topology caches (marker-layer, no decompilation).
  CC.setCalc () (store ^. #callGraphCache) $ do
    let getCG = case cast imp of
          Just ghidraImp -> GhidraCG.getCallGraphCached ghidraImp allFuncRefs
          Nothing -> CG.getCallGraph imp allFuncRefs
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
    timingLog $ "[timing] getCallGraph (" <> show (length allFuncRefs) <> " funcs): " <> show cgTime
    return cg
  CC.setCalc () (store ^. #transposedCallGraphCache) $ do
    cg <- getCallGraph store
    return $ G.transpose cg

  -- Ancestors, descendants, and callSitesToFunc per FuncRef (call-graph layer).
  debug "  [store] Setting up function caches..."
  forM_ allFuncRefs $ \funcRef -> do
    CC.setCalc funcRef (store ^. #ancestorsCache) $ do
      cg <- fromJust <$> CC.get () (store ^. #transposedCallGraphCache)
      return $ G.getStrictDescendants funcRef cg
    CC.setCalc funcRef (store ^. #descendantsCache) $ do
      cg <- fromJust <$> CC.get () (store ^. #callGraphCache)
      return $ G.getStrictDescendants funcRef cg
    CC.setCalc funcRef (store ^. #callSitesToFuncCache) $
      CG.getCallSites imp funcRef

  -- CFG-layer caches (cfgCache, acyclicCfgCache, acyclicDescendantsCache,
  -- callSitesInFuncCache) are NOT pre-registered per function. They use
  -- getOrCompute on demand via the getter functions below.

  whenM (readIORef samplingTimingEnabled) $
    void $ getCallGraph store

  return (store, functionToTypeHintsMap)

-- | Convert a CallNode to a CallSite with the given Function as the caller.
toCallSite :: Function -> Cfg.CallNode [Pil.Stmt] -> Maybe CallSite
toCallSite func n = case n ^. #callDest of
  Pil.CallFunc destFunc -> Just $ CG.CallSite (toFunctionRef func) (n ^. #start) (Func.InternalRef $ toFunctionRef destFunc)
  Pil.CallExtern destExtern -> Just $ CG.CallSite (toFunctionRef func) (n ^. #start) (Func.ExternalRef $ toExternFunctionRef destExtern)
  _ -> Nothing

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

-- | Gets the acyclic CFG for a function, computing on demand via defaultCalc.
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

-- | Gets the CfgInfo for a function, computing on demand via defaultCalc.
getFuncCfgInfo :: CfgStore -> Function -> IO (Maybe CfgInfo)
getFuncCfgInfo store func = join <$> CC.get func (store ^. #cfgCache)

-- | Gets call sites contained within a function (caller's CFG), computing on demand.
getCallSitesInFunc :: CfgStore -> Function -> IO [CallSite]
getCallSitesInFunc store func = fromMaybe [] <$> CC.get func (store ^. #callSitesInFuncCache)

getAncestors :: CfgStore -> FuncRef -> IO (Maybe (HashSet FuncRef))
getAncestors store func = CC.get func $ store ^. #ancestorsCache

-- | Convenience function that extracts internal FunctionRefs from ancestors.
getAncestors' :: CfgStore -> FunctionRef -> IO (Maybe (HashSet FunctionRef))
getAncestors' store fm = mapMaybeHashSet (^? #_InternalRef)
  <<$>> getAncestors store (Func.InternalRef fm)


getDescendants :: CfgStore -> FuncRef -> IO (Maybe (HashSet FuncRef))
getDescendants store func = CC.get func $ store ^. #descendantsCache

-- | Convenience function that extracts internal FunctionRefs from descendants.
getDescendants' :: CfgStore -> FunctionRef -> IO (Maybe (HashSet FunctionRef))
getDescendants' store fm = mapMaybeHashSet (^? #_InternalRef)
  <<$>> getDescendants store (Func.InternalRef fm)


getFuncCfg :: CfgStore -> Function -> IO (Maybe PilCfg)
getFuncCfg store func = fmap (view #cfg) <$> getFuncCfgInfo store func

getCallGraph :: CfgStore -> IO CallGraph
getCallGraph store = fromJust <$> CC.get () (store ^. #callGraphCache)

getFuncs :: CfgStore -> IO [FuncRef]
getFuncs store = fromJust <$> CC.get () (store ^. #funcs)

getInternalFuncs :: CfgStore -> IO [FunctionRef]
getInternalFuncs store = fromJust <$> CC.get () (store ^. #internalFuncs)

getExternalFuncs :: CfgStore -> IO [FunctionRef]
getExternalFuncs store = mapMaybe (^? #_ExternalRef) <$> getFuncs store

-- | Resolve a FunctionRef to a full Function via the lazy decompilation cache.
resolveFunction :: CfgStore -> FunctionRef -> IO (Maybe Function)
resolveFunction store marker = CC.get marker (store ^. #funcCalc)

-- | Resolve a FuncRef to a full Func. Internal functions go through the
-- lazy decompilation cache; external functions go through the extern cache.
resolveFuncRef :: CfgStore -> FuncRef -> IO (Maybe Func)
resolveFuncRef store = \case
  Func.InternalRef fm -> fmap Func.Internal <$> resolveFunction store fm
  Func.ExternalRef fm -> fmap Func.External <$> CC.get fm (store ^. #externFuncCalc)

-- | Get full internal Functions (with params), resolved from funcCalc.
-- Use sparingly — prefer targeted resolution when only a subset is needed.
getInternalFullFuncs :: CfgStore -> IO [Function]
getInternalFullFuncs store = do
  markers <- getInternalFuncs store
  catMaybes <$> mapM (resolveFunction store) markers

-- | Get call sites that target a given function (callee → callers)
getCallSitesToFunc :: CfgStore -> FuncRef -> IO [CallSite]
getCallSitesToFunc store func = fromMaybe [] <$> CC.get func (store ^. #callSitesToFuncCache)

getTransposedCallGraph :: CfgStore -> IO CallGraph
getTransposedCallGraph store = fromJust <$> CC.get () (store ^. #transposedCallGraphCache)

-- | Adds a pre-computed cfg to the store.
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

-- | Eagerly triggers CFG computation for a function. With defaultCalc
-- set on cfgCache, this is equivalent to just calling getFuncCfgInfo.
addFunc :: CfgStore -> Function -> IO ()
addFunc store func = void $ CC.get func (store ^. #cfgCache)

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
      addrToFunc = HashMap.fromList [(f ^. #address, f) | f <- HashMap.keys cfgInfos]
  ancestorsSnapshot <- CC.getSnapshot $ store ^. #ancestorsCache
  let callAncestors = toFunctionAncestorMap addrToFunc ancestorsSnapshot

      outerNodeDescendants = G.calcOuterNodeDescendants (fromMaybe HashSet.empty . flip HashMap.lookup funcInnerNodes) callAncestors

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
    lookupFunc addrMap marker = HashMap.lookup (marker ^. #address) addrMap
    toFunctionAncestorMap :: HashMap Address Function -> HashMap FuncRef (HashSet FuncRef) -> HashMap Function (HashSet Function)
    toFunctionAncestorMap addrMap = HashMap.fromList
      . mapMaybe (\(fm, s) -> fm ^? #_InternalRef >>= lookupFunc addrMap >>= \function ->
                     return ( function
                            , HashSet.fromList
                              . mapMaybe ((^? #_InternalRef) >=> lookupFunc addrMap)
                              . HashSet.toList
                              $ s))
      . HashMap.toList
    getCallTargetFunc :: PilNode -> Maybe Function
    getCallTargetFunc (Cfg.Call n) = CfgI.getTargetFunc n
    getCallTargetFunc _ = Nothing

getFuncNameMapping :: CfgStore -> IO (HashMap Text FunctionRef)
getFuncNameMapping store = do
  markers <- getInternalFuncs store
  return . foldl' (\m marker -> HashMap.insert (marker ^. #name) marker m) HashMap.empty $ markers


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
  prims <- getInitialWMIsFromStore (getFuncs store) (resolveFuncRef store) sprims
  CM.putSnapshot prims $ store ^. #callablePrims

-- | Add new KnownFuncs to the existing callable primitives (merge, not replace).
addKnownFuncs
  :: [KnownFunc]
  -> CfgStore
  -> IO ()
addKnownFuncs newFuncs store = do
  existing <- CM.getSnapshot $ store ^. #callablePrims
  newWMIs <- getInitialWMIsFromStore (getFuncs store) (resolveFuncRef store) newFuncs
  let merged = HashMap.unionWith HashSet.union existing newWMIs
  CM.putSnapshot merged $ store ^. #callablePrims

-- | Replace the set of known-function-derived primitives while preserving
-- any primitives learned dynamically during analysis.
replaceKnownFuncsPreservingLearned
  :: [KnownFunc]
  -> [KnownFunc]
  -> CfgStore
  -> IO ()
replaceKnownFuncsPreservingLearned oldFuncs newFuncs store = do
  existing <- CM.getSnapshot $ store ^. #callablePrims
  oldWMIs <- getInitialWMIsFromStore (getFuncs store) (resolveFuncRef store) oldFuncs
  newWMIs <- getInitialWMIsFromStore (getFuncs store) (resolveFuncRef store) newFuncs
  let learnedOnly = HashMap.differenceWith dropInitial existing oldWMIs
      merged = HashMap.unionWith HashSet.union learnedOnly newWMIs
  CM.putSnapshot merged $ store ^. #callablePrims
  where
    dropInitial current initial =
      let remaining = HashSet.difference current initial
      in if HashSet.null remaining then Nothing else Just remaining
