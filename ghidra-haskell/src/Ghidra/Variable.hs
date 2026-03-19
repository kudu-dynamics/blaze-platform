{-# LANGUAGE DataKinds #-}
module Ghidra.Variable
  ( module Ghidra.Variable
  , HighVarNode
  , HighVariable
  , HighVariableType
  , VarNode
  , VarType(..)
  ) where

import Ghidra.Prelude hiding (toList, Const(Const), mkDataType)

import Foreign.JNI.Types (JObject)
import qualified Language.Java as Java
import qualified Ghidra.Types as J
import Ghidra.Types.Internal (Ghidra, runIO)
import Ghidra.Types.Variable
import Ghidra.Address (Address, mkAddress)
import Ghidra.Util (maybeNullCall, maybeNull)
import qualified Data.Text as Text
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import qualified Data.IntMap.Strict as IntMap
import qualified Foreign.JNI as JNI
import Ghidra.Types.GhidraDataTypes (GhidraDataType)
import Ghidra.GhidraDataTypes (parseDataTypeWithTransaction)

-- | Cache for expensive JNI lookups during high pcode processing.
-- Holds the Address.NO_ADDRESS static field (fetched once instead of per-varnode)
-- and a memoization map for HighVariable objects (many varnodes share the same one).
data HighVarCache = HighVarCache
  { noAddr :: J.Address
  , highVarMapRef :: IORef (IntMap.IntMap HighVariable)
  }

-- | Create a new cache. Call once per function, share across all blocks.
newHighVarCache :: Ghidra HighVarCache
newHighVarCache = do
  na <- runIO $ Java.getStaticField "ghidra.program.model.address.Address" "NO_ADDRESS"
    >>= JNI.newGlobalRef
  ref <- runIO $ newIORef IntMap.empty
  return $ HighVarCache na ref


mkVarType :: Either J.VarNode J.VarNodeAST -> Ghidra VarType
mkVarType (Left v) = runIO (Java.call v "isConstant") >>= \case
  True -> do
    -- value of const is stored in address
    addr :: J.Address <- runIO $ Java.call v "getAddress"
    Const <$> runIO (Java.call addr "getOffset")
  False -> do
    Addr
      <$> (runIO (Java.call v "getAddress" >>= JNI.newGlobalRef) >>= mkAddress)
      <*> pure Nothing
mkVarType (Right v) = runIO (Java.call v "isConstant") >>= \case
  True -> do
    -- value of const is stored in address
    addr :: J.Address <- runIO $ Java.call v "getAddress"
    Const <$> runIO (Java.call addr "getOffset")
  False -> do
    Addr
      <$> (runIO (Java.call v "getAddress" >>= JNI.newGlobalRef) >>= mkAddress)
      <*> do
        pcAddr :: J.Address <- runIO (Java.call v "getPCAddress")
        noAddr :: J.Address <- runIO (Java.getStaticField "ghidra.program.model.address.Address" "NO_ADDRESS")
        if pcAddr == noAddr
          then pure Nothing
          -- call to JNI.newGlobalRef should occur at this point since pcAddr can be discarded otherwise
          else Just <$> do runIO (JNI.newGlobalRef pcAddr) >>= mkAddress

mkVarNode :: J.VarNode -> Ghidra VarNode
mkVarNode v = do
  sz :: Int32 <- runIO $ Java.call v "getSize"
  VarNode <$> mkVarType (Left v) <*> pure (fromIntegral sz)

mkHighVariableType :: J.HighVariable -> Ghidra HighVariableType
mkHighVariableType hv = do
  cls :: J.Class <- runIO $ Java.call (coerce hv :: JObject) "getClass" >>= JNI.newGlobalRef
  name' :: Text <- runIO $ Java.call cls "getName"
                  >>= JNI.newGlobalRef
                  >>= Java.reify
  case lastMay $ Text.splitOn "." name' of
    Nothing -> error "Class name invalid"
    Just cname -> case cname of
      "HighConstant" -> do
        s :: J.Scalar <- runIO $ Java.call (coerce hv :: J.HighConstant) "getScalar"
                  >>= JNI.newGlobalRef
        HighConstant <$> runIO (Java.call s "getValue")
      "HighGlobal" -> return HighGlobal
      "HighLocal" -> return HighLocal
      "HighParam" -> do
        slot :: Int32 <- runIO $ Java.call (coerce hv :: J.HighParam) "getSlot"
        return . HighParam . fromIntegral $ slot
      "HighOther" -> return HighOther
      other -> error $ "Invalid class name: " <> cs other

mkDataType :: J.ProgramDB -> J.HighVariable -> Ghidra GhidraDataType
mkDataType prog hv = do
  dt :: J.DataType <- runIO $ Java.call hv "getDataType"
  parseDataTypeWithTransaction prog dt

getHighSymbol :: J.HighVariable -> Ghidra (Maybe HighSymbol)
getHighSymbol hv = do
  mhsym :: Maybe J.HighSymbol <- maybeNullCall . runIO $ Java.call hv "getSymbol"
  case mhsym of
    Nothing -> return Nothing
    Just hsym -> do
      name :: Maybe Text <- maybeNullCall . runIO $ Java.call (hsym :: J.HighSymbol) "getName" >>= Java.reify
      isParamVar :: Bool <- fmap (fromMaybe False) . maybeNullCall . runIO $ Java.call hsym "isParameter" 
      return . Just $ HighSymbol name isParamVar

mkHighVariable :: J.ProgramDB -> J.HighVariable -> Ghidra HighVariable
mkHighVariable prog hv = do
  sz :: Int32 <- runIO $ Java.call hv "getSize"
  mVarNameStr <- maybeNullCall . runIO $ Java.call hv "getName" >>= Java.reify
  dt <- mkDataType prog hv
  hvt <- mkHighVariableType hv
  mhsym <- getHighSymbol hv
  return $ HighVariable
    { dataType = dt
    , name = mVarNameStr
    , size = fromIntegral sz
    , highVariableType = hvt
    , highSymbol = mhsym
    }

mkHighVarNode :: J.ProgramDB -> J.VarNodeAST -> Ghidra HighVarNode
mkHighVarNode prog v = do
  -- sz :: Int32 <- Java.call (coerce v :: J.VarNode)  "getSize"
  sz :: Int32 <- runIO $ Java.call v "getSize"
  mhv <- maybeNull <$> runIO (Java.call v "getHigh")
  mhv' <- maybe (return Nothing) (fmap Just . (mkHighVariable prog <=< runIO . JNI.newGlobalRef)) mhv
  HighVarNode <$> mkVarType (Right v) <*> pure (fromIntegral sz) <*> getPcAddress <*> pure mhv'

  where
    -- Only works for high varnodes
    -- "getPCAddress" causes nullPointerException if used on low varnode
    getPcAddress :: Ghidra (Maybe Address)
    getPcAddress = do
      addr :: J.Address <- runIO $ Java.call v "getPCAddress"
      noAddress :: J.Address <- runIO $ Java.getStaticField "ghidra.program.model.address.Address" "NO_ADDRESS"
      if addr == noAddress
        then return Nothing
        else fmap Just $ runIO (JNI.newGlobalRef addr) >>= mkAddress

-- | Like 'mkHighVarNode' but uses a cache for NO_ADDRESS lookups and
-- HighVariable deduplication. Multiple varnodes often share the same
-- HighVariable object; caching avoids ~10 redundant JNI calls per duplicate.
mkHighVarNodeCached :: J.ProgramDB -> HighVarCache -> J.VarNodeAST -> Ghidra HighVarNode
mkHighVarNodeCached prog cache v = do
  sz :: Int32 <- runIO $ Java.call v "getSize"
  mhv <- maybeNull <$> runIO (Java.call v "getHigh")
  mhv' <- case mhv of
    Nothing -> return Nothing
    Just hv -> do
      hvRef <- runIO $ JNI.newGlobalRef hv
      key :: Int32 <- runIO $ Java.call hvRef "hashCode"
      cached <- runIO $ readIORef (highVarMapRef cache)
      case IntMap.lookup (fromIntegral key) cached of
        Just result -> return (Just result)
        Nothing -> do
          result <- mkHighVariable prog hvRef
          runIO $ modifyIORef' (highVarMapRef cache) (IntMap.insert (fromIntegral key) result)
          return (Just result)
  HighVarNode <$> mkVarTypeCached (noAddr cache) v <*> pure (fromIntegral sz) <*> getPcAddress <*> pure mhv'
  where
    getPcAddress :: Ghidra (Maybe Address)
    getPcAddress = do
      addr :: J.Address <- runIO $ Java.call v "getPCAddress"
      if addr == noAddr cache
        then return Nothing
        else fmap Just $ runIO (JNI.newGlobalRef addr) >>= mkAddress

-- | Like 'mkVarType' for the Right (high varnode) case, but uses a cached NO_ADDRESS.
mkVarTypeCached :: J.Address -> J.VarNodeAST -> Ghidra VarType
mkVarTypeCached cachedNoAddr v = runIO (Java.call v "isConstant") >>= \case
  True -> do
    addr :: J.Address <- runIO $ Java.call v "getAddress"
    Const <$> runIO (Java.call addr "getOffset")
  False -> do
    Addr
      <$> (runIO (Java.call v "getAddress" >>= JNI.newGlobalRef) >>= mkAddress)
      <*> do
        pcAddr :: J.Address <- runIO (Java.call v "getPCAddress")
        if pcAddr == cachedNoAddr
          then pure Nothing
          else Just <$> do runIO (JNI.newGlobalRef pcAddr) >>= mkAddress


