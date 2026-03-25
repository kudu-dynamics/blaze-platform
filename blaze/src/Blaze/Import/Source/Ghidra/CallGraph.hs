module Blaze.Import.Source.Ghidra.CallGraph where

import Blaze.Prelude hiding (Symbol)

import Blaze.Import.Source.Ghidra.Types (
  convertAddress,
  GhidraImporter(GhidraImporter)
  )
import Blaze.Types.CallGraph (CallSite(CallSite))
import qualified Blaze.Types.CachedMap as CM
import qualified Blaze.Types.CallGraph as CG
import Blaze.Types.Function (
  ExternFunction,
  Function,
  Func (Internal, External),
  FuncParamInfo (FuncParamInfo, FuncVarArgInfo),
  ParamInfo (ParamInfo),
 )
import qualified Blaze.Types.Function as BFunc

import Ghidra.Core (runGhidraOrError)
import qualified Ghidra.State as State
import qualified Ghidra.Function as G
import qualified Ghidra.Types.Function as G
import Ghidra.Types (Ghidra)
import qualified Ghidra.Types as J
import qualified Ghidra.Reference as GRef
import qualified Ghidra.Address as Addr

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (nub)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.BinaryAnalysis as BA
import qualified Blaze.Types.Graph as G
import Control.Concurrent.Async (forConcurrently)


getFuncAddr :: G.Function -> Address
getFuncAddr = convertAddress . view #startAddress

convertParam :: G.HighParameter -> ParamInfo
convertParam p = ParamInfo (p ^. #name) (Just . fromIntegral $ p ^. #size) BFunc.Unknown

toGhidraFunction :: J.ProgramDB -> Function -> IO J.Function
toGhidraFunction prg fn = getJFunction prg (fn ^. #address) >>= \case
    Nothing -> error $ "Couldn't find function at addr: " <> show (fn ^. #address)
    Just fn' -> return fn'

getJFunction :: J.ProgramDB -> Address -> IO (Maybe J.Function)
getJFunction prg addr = runGhidraOrError $ do
  jaddr <- State.mkAddress prg addr
  G.fromAddr prg jaddr

type ThunkMap = HashMap Int64 J.Function

getFuncFromJFunction_ :: GhidraImporter -> IORef (Maybe ThunkMap) -> J.Function -> IO Func
getFuncFromJFunction_ imp@(GhidraImporter gs _ _) thunkMapRef jfunc = do
  let prg = gs ^. #program
  (dethunkedJFunc, isExt) <- runGhidraOrError $ do
    dethunkedJFunc <- G.resolveThunk jfunc
    isExt <- G.isExternal dethunkedJFunc
    return (dethunkedJFunc, isExt)
  case isExt of
    True -> fmap External . runGhidraOrError $ mkExternFunc dethunkedJFunc
    False -> do
      hasDflt <- runGhidraOrError $ G.hasDefaultName jfunc
      if hasDflt
        then tryResolveAsExternThunk thunkMapRef prg jfunc >>= \case
          Just externFunc -> return $ External externFunc
          Nothing -> Internal <$> mkInternalFunc imp dethunkedJFunc
        else Internal <$> mkInternalFunc imp dethunkedJFunc

getFuncFromJFunction :: GhidraImporter -> J.Function -> IO Func
getFuncFromJFunction imp jfunc = do
  ref <- newIORef Nothing
  getFuncFromJFunction_ imp ref jfunc

tryResolveAsExternThunk :: IORef (Maybe ThunkMap) -> J.ProgramDB -> J.Function -> IO (Maybe ExternFunction)
tryResolveAsExternThunk thunkMapRef prg jfunc = runGhidraOrError $ do
  bodyAddrs <- J.toAddrs jfunc
  allRefs <- concat <$> traverse (GRef.getReferencesFromAddress prg) bodyAddrs
  refTargetOffsets <- forM allRefs $
    fmap (view #offset) . (Addr.mkAddress <=< GRef.getToAddress)
  thunkMap <- liftIO (readIORef thunkMapRef) >>= \case
    Just m -> return m
    Nothing -> do
      externFuncs <- G.getExternalFunctions prg
      m <- foldM buildMap HashMap.empty externFuncs
      liftIO $ writeIORef thunkMapRef (Just m)
      return m
  case mapMaybe (`HashMap.lookup` thunkMap) refTargetOffsets of
    (ef:_) -> Just <$> mkExternFunc ef
    [] -> return Nothing
  where
    buildMap m ef = do
      offsets <- traverse (fmap (view #offset) . Addr.mkAddress) =<< G.getFunctionThunkAddresses ef
      return $ foldl' (\m' o -> HashMap.insert o ef m') m offsets

getFunction :: GhidraImporter -> Address -> IO (Maybe Func)
getFunction imp@(GhidraImporter gs _ _) addr = getJFunction (gs ^. #program) addr >>= \case
  Nothing -> return Nothing
  Just jfunc -> Just <$> getFuncFromJFunction imp jfunc

getHighFunction :: GhidraImporter -> Address -> J.Function -> IO J.HighFunction
getHighFunction (GhidraImporter gs fc _) addr fn = CM.get addr fc >>= \case
  Nothing -> do
    hf <- runGhidraOrError $ G.getHighFunction gs fn
    CM.set addr (Just hf) fc
    return hf
  Just cachedFn -> return cachedFn

convertRawParam :: G.Parameter -> BFunc.FuncParamInfo
convertRawParam p = BFunc.FuncParamInfo $ BFunc.ParamInfo (p ^. #name) Nothing BFunc.Unknown

-- | Converts Ghidra function to a Blaze internal (not extern) Function
-- G.isExternal must be True
mkInternalFunc :: GhidraImporter -> J.Function -> IO Function
mkInternalFunc imp jfunc = do
  addr <- convertAddress <$> runGhidraOrError (G.getAddress jfunc)
  highFn <- getHighFunction imp addr jfunc
  runGhidraOrError $ do
    name <- G.getName jfunc
    proto <- G.getProto highFn
    isVariadic <- G.isVarArg proto
    paramInfos <- fmap convertParam <$> G.getHighParams highFn
    -- Don't really know how to tell if individual params are varargs
    -- so for now, if isVariadic is true, we use FuncVarArgInfo
    let params = bool FuncParamInfo FuncVarArgInfo isVariadic <$> paramInfos
    return
      BFunc.Function
        { symbol = Nothing
        , name = name
        , address = addr
        , params = params
        }

-- | Converts Ghidra function to a Blaze ExternFunction
-- G.isExternal must be True
mkExternFunc :: J.Function -> Ghidra ExternFunction
mkExternFunc jfunc = do
  gaddr <- G.getAddress jfunc
  name <- G.getName jfunc -- hopefully will always have name
  mLibraryName <- G.getExternalLocation jfunc >>= maybe (return Nothing) G.getLibraryName
  params <- G.getLowParams jfunc
  let addr = Address
        { BA.space = AddressSpace
          { ptrSize = Bytes 8
          , addressableUnitSize = Bytes 1
          , name = BA.EXTERNAL
          }
        , BA.offset = fromIntegral $ gaddr ^. #offset
        }
  return $ BFunc.ExternFunction
    { symbol = Nothing
    , name = name
    , library = mLibraryName
    , address = addr
    , params = convertRawParam <$> params
    }

getFunctions :: GhidraImporter -> IO [Func]
getFunctions imp@(GhidraImporter gs _ _) = do
  thunkMapRef <- newIORef Nothing
  fmap nub $ runGhidraOrError (G.getFunctions' opts $ gs ^. #program)
    >>= traverse (getFuncFromJFunction_ imp thunkMapRef)
  where
    -- Are these sensible options for blaze?
    opts = G.GetFunctionsOptions
      { G.includeExternalFuncs = True
      , G.includeLocalFuncs = True
      , G.excludeDefaultFuncs = False
      , G.excludeThunks = True
      , G.resolveThunks = True
      }

-- | Gets all callsites that call fn.
-- For internal functions, uses reference-based lookup.
-- For extern functions, uses Ghidra's getCallingFunctions which follows through thunks/PLT stubs.
getCallSites :: GhidraImporter -> Func -> IO [CallSite]
getCallSites imp@(GhidraImporter gs _ _) fn = do
  let prg = gs ^. #program
  mgfunc <- case fn of
    BFunc.Internal func -> do
      startAddr <- runGhidraOrError . State.mkAddress prg $ func ^. #address
      runGhidraOrError $ G.fromAddr prg startAddr
    BFunc.External func -> do
      externAddr <- runGhidraOrError
        . State.mkExternalAddress prg
        . fromIntegral
        $ func ^. #address . #offset
      runGhidraOrError $ G.getFunctionAt prg externAddr

  case mgfunc of
    Nothing -> error $ "Could not find callee function for func: " <> show fn
    Just gfunc -> case fn of
      -- For internal functions, use reference-based lookup (existing behavior)
      BFunc.Internal _ ->
        runGhidraOrError (GRef.getFunctionRefs prg gfunc) >>= mapMaybeM mkCallSiteFromRef
      -- For extern functions, find call sites via thunk references.
      -- Extern calls go through PLT/thunk stubs, so we find the thunks for
      -- the extern and then use reference-based lookup (like the internal branch)
      -- to get the actual call instruction addresses within each caller.
      BFunc.External _ -> do
        thunkAddrs <- runGhidraOrError $ G.getFunctionThunkAddresses gfunc
        thunkRefs <- fmap concat . forM thunkAddrs $ \thunkAddr -> do
          mthunk <- runGhidraOrError $ G.fromAddr prg thunkAddr
          case mthunk of
            Nothing -> return []
            Just thunkFunc ->
              runGhidraOrError (GRef.getFunctionRefs prg thunkFunc) >>= mapMaybeM mkCallSiteFromRef
        -- Also try direct references to the extern itself (no thunk)
        directRefs <- runGhidraOrError (GRef.getFunctionRefs prg gfunc) >>= mapMaybeM mkCallSiteFromRef
        return . nub $ thunkRefs <> directRefs
  where
    mkCallSiteFromRef :: GRef.FuncRef -> IO (Maybe CallSite)
    mkCallSiteFromRef x = do
      caller <- getFuncFromJFunction imp $ x ^. #caller . #handle
      case caller of
        External _ -> return Nothing
        Internal func -> do
          let addr = convertAddress $ x ^. #callerAddr
          return . Just $ CallSite { CG.caller = func
                                   , CG.address = addr
                                   , CG.dest = fn
                                   }

-- | Like 'getCallSites' but uses a pre-built function cache to avoid
-- expensive JNI round-trips in 'getFuncFromJFunction'. The same caller
-- function appears in many references; the cache turns ~15 JNI calls per
-- reference into a single HashMap lookup.
type FuncCache = HashMap Address Func

getCallSitesCached :: GhidraImporter -> FuncCache -> Func -> IO [CallSite]
getCallSitesCached (GhidraImporter gs _ _) funcCache fn = do
  let prg = gs ^. #program
  mgfunc <- case fn of
    BFunc.Internal func -> do
      startAddr <- runGhidraOrError . State.mkAddress prg $ func ^. #address
      runGhidraOrError $ G.fromAddr prg startAddr
    BFunc.External func -> do
      externAddr <- runGhidraOrError
        . State.mkExternalAddress prg
        . fromIntegral
        $ func ^. #address . #offset
      runGhidraOrError $ G.getFunctionAt prg externAddr

  case mgfunc of
    Nothing -> error $ "Could not find callee function for func: " <> show fn
    Just gfunc -> case fn of
      BFunc.Internal _ ->
        runGhidraOrError (GRef.getFunctionRefs prg gfunc) >>= mapMaybeM mkCallSiteFromRef
      BFunc.External _ -> do
        thunkAddrs <- runGhidraOrError $ G.getFunctionThunkAddresses gfunc
        thunkRefs <- fmap concat . forM thunkAddrs $ \thunkAddr -> do
          mthunk <- runGhidraOrError $ G.fromAddr prg thunkAddr
          case mthunk of
            Nothing -> return []
            Just thunkFunc ->
              runGhidraOrError (GRef.getFunctionRefs prg thunkFunc) >>= mapMaybeM mkCallSiteFromRef
        directRefs <- runGhidraOrError (GRef.getFunctionRefs prg gfunc) >>= mapMaybeM mkCallSiteFromRef
        return . nub $ thunkRefs <> directRefs
  where
    mkCallSiteFromRef :: GRef.FuncRef -> IO (Maybe CallSite)
    mkCallSiteFromRef x = do
      -- Look up the caller by its entry address in the cache instead of
      -- calling getFuncFromJFunction (~15 JNI calls per reference).
      let callerEntryAddr = convertAddress $ x ^. #caller . #startAddress
      case HashMap.lookup callerEntryAddr funcCache of
        Just (Internal func) -> do
          let addr = convertAddress $ x ^. #callerAddr
          return . Just $ CallSite { CG.caller = func
                                   , CG.address = addr
                                   , CG.dest = fn
                                   }
        _ -> return Nothing  -- external or unknown caller, skip

-- | Build a call graph using a function cache for fast caller lookups.
-- This avoids redundant JNI calls when the same function appears as a
-- caller in multiple references.
getCallGraphCached :: GhidraImporter -> [Func] -> IO CG.CallGraph
getCallGraphCached imp funcs = do
  let funcCache :: FuncCache
      funcCache = HashMap.fromList
        [ (addr, f)
        | f <- funcs
        , let addr = case f of
                BFunc.Internal func -> func ^. #address
                BFunc.External func -> func ^. #address
        ]
  edges <- fmap concat . forConcurrently funcs $ \callee ->
    fmap (\callSite -> (Internal $ callSite ^. #caller, callee))
      <$> getCallSitesCached imp funcCache callee
  pure . G.addNodes funcs . G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $ edges
