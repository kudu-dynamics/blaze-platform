module Blaze.Import.Source.Ghidra.CallGraph where

import Blaze.Prelude hiding (Symbol)

import Blaze.Import.Source.Ghidra.Types (
  convertAddress,
  GhidraImporter
  )
import Blaze.Types.CallGraph (CallSite(CallSite))
import qualified Blaze.Types.CachedCalc as CC
import qualified Blaze.Types.CallGraph as CG
import Blaze.Types.Function (
  ExternFunction,
  Function,
  Func (Internal, External),
  FuncRef (InternalRef),
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

-- | Convert a JFunction to a FuncRef WITHOUT decompiling.
-- Used by 'getFunctions' for the initial function list and call graph construction.
getFuncRefFromJFunction :: J.Function -> IO BFunc.FuncRef
getFuncRefFromJFunction jfunc = do
  (dethunkedJFunc, isExt) <- runGhidraOrError $ do
    dethunkedJFunc <- G.resolveThunk jfunc
    isExt <- G.isExternal dethunkedJFunc
    return (dethunkedJFunc, isExt)
  case isExt of
    True -> fmap (BFunc.ExternalRef . BFunc.toExternFunctionRef) . runGhidraOrError $ mkExternFunc dethunkedJFunc
    False -> BFunc.InternalRef <$> mkFunctionRef dethunkedJFunc

-- | Convert a JFunction to a Func WITH full decompilation for params.
getFuncFromJFunction :: GhidraImporter -> J.Function -> IO Func
getFuncFromJFunction imp jfunc = do
  (dethunkedJFunc, isExt) <- runGhidraOrError $ do
    dethunkedJFunc <- G.resolveThunk jfunc
    isExt <- G.isExternal dethunkedJFunc
    return (dethunkedJFunc, isExt)
  case isExt of
    True -> fmap External . runGhidraOrError $ mkExternFunc dethunkedJFunc
    False -> Internal <$> mkInternalFunc imp dethunkedJFunc

getFunction :: GhidraImporter -> Address -> IO (Maybe Func)
getFunction imp addr = do
  let prg = imp ^. #ghidraState . #program
  mjfunc <- case addr ^. #space . #name of
    BA.EXTERNAL -> runGhidraOrError
      $ State.mkExternalAddress prg (fromIntegral $ addr ^. #offset)
      >>= G.getFunctionAt prg
    _ -> getJFunction prg addr
  case mjfunc of
    Nothing -> return Nothing
    Just jfunc -> Just <$> getFuncFromJFunction imp jfunc

getHighFunction :: GhidraImporter -> Address -> J.Function -> IO J.HighFunction
getHighFunction imp addr jfunc =
  CC.getOrCompute addr computeHF (imp ^. #highFnCalc)
  where
    computeHF = runGhidraOrError $ G.getHighFunction (imp ^. #ghidraState) jfunc

convertRawParam :: G.Parameter -> BFunc.FuncParamInfo
convertRawParam p = BFunc.FuncParamInfo $ BFunc.ParamInfo (p ^. #name) Nothing BFunc.Unknown

-- | Converts Ghidra function to a Blaze internal (not extern) Function,
-- with full params from decompilation.
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

-- | Lightweight marker from a Ghidra function — no decompilation needed.
mkFunctionRef :: J.Function -> IO BFunc.FunctionRef
mkFunctionRef jfunc = runGhidraOrError $ do
  addr <- convertAddress <$> G.getAddress jfunc
  name <- G.getName jfunc
  return BFunc.FunctionRef
    { symbol = Nothing
    , name = name
    , address = addr
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

getFunctions :: GhidraImporter -> IO [BFunc.FuncRef]
getFunctions imp = do
  fmap nub $ runGhidraOrError (G.getFunctions' opts $ imp ^. #ghidraState . #program)
    >>= traverse getFuncRefFromJFunction
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
-- Uses reference-based lookup plus thunk-following for both internal and extern functions.
getCallSites :: GhidraImporter -> BFunc.FuncRef -> IO [CallSite]
getCallSites imp fn = do
  let prg = imp ^. #ghidraState . #program
  mgfunc <- case fn of
    BFunc.InternalRef fm -> do
      startAddr <- runGhidraOrError . State.mkAddress prg $ fm ^. #address
      runGhidraOrError $ G.fromAddr prg startAddr
    BFunc.ExternalRef fm -> do
      externAddr <- runGhidraOrError
        . State.mkExternalAddress prg
        . fromIntegral
        $ fm ^. #address . #offset
      runGhidraOrError $ G.getFunctionAt prg externAddr

  case mgfunc of
    Nothing -> error $ "Could not find callee function for func: " <> show fn
    Just gfunc -> do
      -- Direct references to the function itself
      directRefs <- runGhidraOrError (GRef.getFunctionRefs prg gfunc) >>= mapMaybeM mkCallSiteFromRef
      -- Also find references through thunks — callers may reference a thunk
      -- rather than the function directly (PLT stubs for externs, or internal thunks).
      thunkAddrs <- runGhidraOrError $ G.getFunctionThunkAddresses gfunc
      thunkRefs <- fmap concat . forM thunkAddrs $ \thunkAddr -> do
        mthunk <- runGhidraOrError $ G.fromAddr prg thunkAddr
        case mthunk of
          Nothing -> return []
          Just thunkFunc ->
            runGhidraOrError (GRef.getFunctionRefs prg thunkFunc) >>= mapMaybeM mkCallSiteFromRef
      return . nub $ directRefs <> thunkRefs
  where
    mkCallSiteFromRef :: GRef.FuncRef -> IO (Maybe CallSite)
    mkCallSiteFromRef x = do
      callerRef <- getFuncRefFromJFunction $ x ^. #caller . #handle
      case callerRef of
        BFunc.ExternalRef _ -> return Nothing
        BFunc.InternalRef fm -> do
          let addr = convertAddress $ x ^. #callerAddr
          return . Just $ CallSite { CG.caller = fm
                                   , CG.address = addr
                                   , CG.dest = fn
                                   }

-- | Like 'getCallSites' but uses a pre-built FuncRef cache to avoid
-- expensive JNI round-trips. The same caller function appears in many
-- references; the cache turns multiple JNI calls per reference into a
-- single HashMap lookup.
type FuncRefCache = HashMap Address BFunc.FuncRef

getCallSitesCached :: GhidraImporter -> FuncRefCache -> BFunc.FuncRef -> IO [CallSite]
getCallSitesCached imp funcCache fn = do
  let prg = imp ^. #ghidraState . #program
  mgfunc <- case fn of
    BFunc.InternalRef fm -> do
      startAddr <- runGhidraOrError . State.mkAddress prg $ fm ^. #address
      runGhidraOrError $ G.fromAddr prg startAddr
    BFunc.ExternalRef fm -> do
      externAddr <- runGhidraOrError
        . State.mkExternalAddress prg
        . fromIntegral
        $ fm ^. #address . #offset
      runGhidraOrError $ G.getFunctionAt prg externAddr

  case mgfunc of
    Nothing -> error $ "Could not find callee function for func: " <> show fn
    Just gfunc -> do
      directRefs <- runGhidraOrError (GRef.getFunctionRefs prg gfunc) >>= mapMaybeM mkCallSiteFromRef
      thunkAddrs <- runGhidraOrError $ G.getFunctionThunkAddresses gfunc
      thunkRefs <- fmap concat . forM thunkAddrs $ \thunkAddr -> do
        mthunk <- runGhidraOrError $ G.fromAddr prg thunkAddr
        case mthunk of
          Nothing -> return []
          Just thunkFunc ->
            runGhidraOrError (GRef.getFunctionRefs prg thunkFunc) >>= mapMaybeM mkCallSiteFromRef
      return . nub $ directRefs <> thunkRefs
  where
    mkCallSiteFromRef :: GRef.FuncRef -> IO (Maybe CallSite)
    mkCallSiteFromRef x = do
      let callerEntryAddr = convertAddress $ x ^. #caller . #startAddress
      case HashMap.lookup callerEntryAddr funcCache of
        Just (BFunc.InternalRef fm) -> do
          let addr = convertAddress $ x ^. #callerAddr
          return . Just $ CallSite { CG.caller = fm
                                   , CG.address = addr
                                   , CG.dest = fn
                                   }
        _ -> return Nothing  -- external, thunk, or unknown caller, skip

-- | Build a call graph using a function marker cache for fast caller lookups.
getCallGraphCached :: GhidraImporter -> [BFunc.FuncRef] -> IO CG.CallGraph
getCallGraphCached imp funcRefs = do
  let funcCache :: FuncRefCache
      funcCache = HashMap.fromList
        [ (BFunc.funcRefAddress fm, fm)
        | fm <- funcRefs
        ]
  edges <- fmap concat . forConcurrently funcRefs $
    fmap (fmap (\callSite -> (InternalRef $ callSite ^. #caller, callSite ^. #dest)))
      . getCallSitesCached imp funcCache
  pure . G.addNodes funcRefs . G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $ edges
