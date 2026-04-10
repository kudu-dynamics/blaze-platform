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
import Ghidra.Address (Address(Address), AddressSpace, mkAddress, mkAddressSpace, mkAddressFromParts)
import Ghidra.Types.Address (AddressSpaceMap, AddressSpaceId(..))
import qualified Data.Vector.Storable as VS
import Ghidra.Util (maybeNullCall, maybeNull)
import qualified Data.Text as Text
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import qualified Data.IntMap.Strict as IntMap
import qualified Data.HashMap.Strict as HashMap
import qualified Foreign.JNI as JNI
import Ghidra.Types.GhidraDataTypes (GhidraDataType)
import Ghidra.GhidraDataTypes (parseDataTypeWithTransaction)

-- | Cache for expensive JNI lookups during high pcode processing.
-- Holds the Address.NO_ADDRESS static field (fetched once instead of per-varnode),
-- a memoization map for HighVariable objects (many varnodes share the same one),
-- and a lazily-extended AddressSpaceMap for spaces not in the initial map.
data HighVarCache = HighVarCache
  { noAddr :: J.Address
  , highVarMapRef :: IORef (IntMap.IntMap HighVariable)
  , addrSpaceMapRef :: IORef AddressSpaceMap
  }

-- | Create a new cache. Call once per function, share across all blocks.
-- Seeds the address space map with the initial map from GhidraState;
-- unknown spaces encountered during processing are lazily added via JNI.
newHighVarCache :: AddressSpaceMap -> Ghidra HighVarCache
newHighVarCache initialMap = do
  na <- runIO $ Java.getStaticField "ghidra.program.model.address.Address" "NO_ADDRESS"
    >>= JNI.newGlobalRef
  ref <- runIO $ newIORef IntMap.empty
  spaceRef <- runIO $ newIORef initialMap
  return $ HighVarCache na ref spaceRef

-- | Look up an address space by ID, lazily fetching from JNI on first miss.
-- Once fetched, the space is cached for future lookups.
lookupOrFetchSpace :: HighVarCache -> J.VarNodeAST -> Int64 -> Ghidra AddressSpace
lookupOrFetchSpace cache v spaceIdRaw = do
  let spaceId = fromIntegral spaceIdRaw :: AddressSpaceId
  spaceMap <- runIO $ readIORef (addrSpaceMapRef cache)
  case HashMap.lookup spaceId spaceMap of
    Just space -> pure space
    Nothing -> do
      -- Fetch the real AddressSpace via JNI (once per unknown space ID)
      jAddr :: J.Address <- runIO $ Java.call v "getAddress"
      space <- runIO (Java.call jAddr "getAddressSpace") >>= mkAddressSpace
      runIO $ modifyIORef' (addrSpaceMapRef cache) (HashMap.insert spaceId space)
      pure space


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
        -- HighConstant.getScalar() calls new Scalar(bitLength, value) which
        -- throws if bitLength == 0.  Some Ghidra HighConstants (e.g. void-typed
        -- pointer constants) have size 0.  Guard against this by checking size
        -- first and falling back to the representative varnode's address offset.
        sz :: Int32 <- runIO $ Java.call hv "getSize"
        if sz > 0
          then do
            s :: J.Scalar <- runIO $ Java.call (coerce hv :: J.HighConstant) "getScalar"
                      >>= JNI.newGlobalRef
            HighConstant <$> runIO (Java.call s "getValue")
          else do
            rep :: J.VarNode <- runIO $ Java.call hv "getRepresentative" >>= JNI.newGlobalRef
            addr :: J.Address <- runIO $ Java.call rep "getAddress" >>= JNI.newGlobalRef
            HighConstant <$> runIO (Java.call addr "getOffset")
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

-- | Like 'mkHighVarNode' but uses a cache for HighVariable deduplication and
-- a Java-side helper to extract all varnode properties in one JNI call.
-- Multiple varnodes often share the same HighVariable object; caching avoids
-- ~10 redundant JNI calls per duplicate.
mkHighVarNodeCached :: J.ProgramDB -> HighVarCache -> J.VarNodeAST -> Ghidra HighVarNode
mkHighVarNodeCached prog cache v = do
  -- One JNI call extracts all basic properties (replaces ~8-25 individual calls)
  basics :: VS.Vector Int64 <- runIO $ Java.callStatic "PcodeHelper" "extractVarNodeBasics" v
                                 >>= Java.reify
  let sz        = basics VS.! 0
      isConst   = basics VS.! 1
      addrOff   = basics VS.! 2
      addrSpId  = basics VS.! 3
      hasPcAddr = basics VS.! 4
      pcOff     = basics VS.! 5
      pcSpId    = basics VS.! 6
      hasHigh   = basics VS.! 7
      highHash  = basics VS.! 8

  -- Helper: build Address from offset + spaceId, lazily caching unknown spaces
  let mkAddr off spId = do
        spaceMap <- runIO $ readIORef (addrSpaceMapRef cache)
        case mkAddressFromParts spaceMap off (fromIntegral spId) of
          Just a  -> pure a
          Nothing -> do
            -- Unknown space — fetch via JNI (once per space, then cached)
            space <- lookupOrFetchSpace cache v spId
            pure $ Address space off

  -- Build VarType
  varType <- if isConst == 1
    then pure $ Const addrOff
    else Addr <$> mkAddr addrOff addrSpId
              <*> (if hasPcAddr == 1 then Just <$> mkAddr pcOff pcSpId else pure Nothing)

  -- PC address for HighVarNode
  pcAddress <- if hasPcAddr == 1
    then Just <$> mkAddr pcOff pcSpId
    else pure Nothing

  -- Handle HighVariable with caching
  mhv' <- if hasHigh == 0
    then pure Nothing
    else do
      cached <- runIO $ readIORef (highVarMapRef cache)
      case IntMap.lookup (fromIntegral highHash) cached of
        Just result -> pure (Just result)
        Nothing -> do
          hv <- runIO $ Java.call v "getHigh" >>= JNI.newGlobalRef
          result <- mkHighVariable prog hv
          runIO $ modifyIORef' (highVarMapRef cache) (IntMap.insert (fromIntegral highHash) result)
          pure (Just result)
  pure $ HighVarNode varType (fromIntegral sz) pcAddress mhv'


