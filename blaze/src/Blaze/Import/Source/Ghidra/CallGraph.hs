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
import Ghidra.State (GhidraState)
import qualified Ghidra.State as State
import qualified Ghidra.Function as G
import qualified Ghidra.Types.Function as G
import Ghidra.Types (Ghidra)
import qualified Ghidra.Types as J
import qualified Ghidra.Reference as GRef

import Data.List (nub)
import qualified Data.BinaryAnalysis as BA


getFuncAddr :: G.Function -> Address
getFuncAddr = convertAddress . view #startAddress

convertParam :: G.HighParameter -> ParamInfo
convertParam p = ParamInfo (p ^. #name) BFunc.Unknown

toGhidraFunction :: GhidraState -> Function -> IO J.Function
toGhidraFunction gs fn = getJFunction gs (fn ^. #address) >>= \case
    Nothing -> error $ "Couldn't find function at addr: " <> show (fn ^. #address)
    Just fn' -> return fn'

getJFunction :: GhidraState -> Address -> IO (Maybe J.Function)
getJFunction gs addr = runGhidraOrError $ do
  jaddr <- State.mkAddress gs addr
  G.fromAddr gs jaddr

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
getFunction imp@(GhidraImporter gs _) addr = getJFunction gs addr >>= \case
  Nothing -> return Nothing
  Just jfunc -> Just <$> getFuncFromJFunction imp jfunc

getHighFunction :: GhidraImporter -> Address -> J.Function -> IO J.HighFunction
getHighFunction (GhidraImporter gs fc) addr fn = CM.get addr fc >>= \case
  Nothing -> do
    hf <- runGhidraOrError $ G.getHighFunction gs fn
    CM.set addr (Just hf) fc
    return hf
  Just cachedFn -> return cachedFn

convertRawParam :: G.Parameter -> BFunc.FuncParamInfo
convertRawParam p = BFunc.FuncParamInfo $ BFunc.ParamInfo (p ^. #name) BFunc.Unknown

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
  mLibraryName <- G.getExternalLocation jfunc >>= G.getLibraryName
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
getFunctions imp@(GhidraImporter gs _) = fmap nub $ runGhidraOrError (G.getFunctions' opts gs)
  >>= traverse (getFuncFromJFunction imp)
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
getCallSites :: GhidraImporter -> Func -> IO [CallSite]
getCallSites imp@(GhidraImporter gs _) fn = do
  mgfunc <- case fn of
    BFunc.Internal func -> do
      startAddr <- runGhidraOrError . State.mkAddress gs $ func ^. #address
      runGhidraOrError (G.fromAddr gs startAddr)
    -- Rudy TODO: clarify what's going on here. mkExternalAddress says it takes an offset,
    -- but what does that actually mean for an external address in Ghidra
    BFunc.External func -> do
      externAddr <- runGhidraOrError
        . State.mkExternalAddress gs
        . fromIntegral
        $ func ^. #address . #offset
      runGhidraOrError (G.getFunctionAt gs externAddr)
      -- externAddr <- runGhidraOrError
      --   . State.mkExternalAddress gs
      --   . fromIntegral
      --   $ func ^. #address . #externalIndex
      -- runGhidraOrError (G.getFunctionAt gs externAddr)

  case mgfunc of
    Nothing -> error $ "Could not find callee function for func: " <> show fn
    Just gfunc -> runGhidraOrError (GRef.getFunctionRefs gs gfunc) >>= mapMaybeM f
  where
    f :: GRef.FuncRef -> IO (Maybe CallSite)
    f x = do
      caller <- getFuncFromJFunction imp $ x ^. #caller . #handle
      case caller of
        External _ -> return Nothing
        Internal func -> do
          let addr = convertAddress $ x ^. #callerAddr
              dest = fn
          return
            . Just
            $ CallSite { CG.caller = func
                       , CG.address = addr
                       , CG.dest = dest
                       }
