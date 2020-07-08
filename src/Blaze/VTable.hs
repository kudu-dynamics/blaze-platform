module Blaze.VTable where

import qualified Binja.Core as BN
import Binja.Core (BNBinaryReader, BNBinaryView, getReaderPosition, read64, read8, seekBinaryReader)
import qualified Binja.Function as BF
import Binja.Function (getFunctionStartingAt)
import Binja.View (getAddressSize, getDefaultReader)
import Blaze.CallGraph (Function)
import Blaze.Import.Source.BinaryNinja (convertFunction)
import Blaze.Prelude
import qualified Blaze.Types.Pil as Pil
import qualified Blaze.Types.VTable as VTable
import Blaze.Types.VTable
  ( TypeInfo (TypeInfo, _helperClass, _name, _parentsTypeInfo),
    VTContext (VTContext, _bv, _reader, _width),
    VTable (VTable, _parents, _topOffset, _typeInfo, _vFunctions, _vptrAddress),
  )
import qualified Data.ByteString as BS
import Data.Text (pack)

getTopOffset_ :: Address -> VTable.Ctx (Maybe Bytes)
getTopOffset_ vptr = do
  ctx <- ask
  let (AddressWidth bitW) = ctx ^. VTable.width
  liftIO $ seekBinaryReader (ctx ^. VTable.reader) (fromIntegral $ vptr - toAddress 2 * fromIntegral bitW)
  case ctx ^. VTable.width of
    (AddressWidth 8) -> fmap (fmap Bytes) . liftIO $ read64 (ctx ^. VTable.reader)
    _ -> return Nothing
  where
    toAddress :: Integer -> Address
    toAddress = Address . fromIntegral

createTypeInfo_ :: Address -> VTable.Ctx (Maybe VTable.TypeInfo)
createTypeInfo_ (Address typeInfoPtr)
  | typeInfoPtr == 0 = return Nothing
  | otherwise = do
    ctx <- ask
    let (AddressWidth bitW) = ctx ^. VTable.width
    helperClassPtr <- liftIO $
      seekAndRead (ctx ^. VTable.reader) (ctx ^. VTable.width) typeInfoPtr >>= \case
        Nothing -> return Nothing
        Just p -> return . Just . Address . Bytes $ p
    name <- liftIO $
      seekAndRead (ctx ^. VTable.reader) (ctx ^. VTable.width) (typeInfoPtr + toBytes bitW) >>= \case
        Nothing -> return $ pack ""
        Just p -> readName_ ctx . Address . Bytes $ p
    parentTypeInfoPtr <- liftIO $ seekAndRead (ctx ^. VTable.reader) (ctx ^. VTable.width) (typeInfoPtr + 2 * toBytes bitW)
    parentTypeInfo <- case parentTypeInfoPtr of
      Nothing -> return Nothing
      Just p -> createTypeInfo_ . Address . Bytes $ p
    return $
      Just
        ( TypeInfo
            { _helperClass = helperClassPtr,
              _name = Just name,
              _parentsTypeInfo = parentTypeInfo
            }
        )
  where
    seekAndRead :: BNBinaryReader -> AddressWidth -> Bytes -> IO (Maybe Word64)
    seekAndRead br width addr = do
      seekBinaryReader br addr
      case width of
        (AddressWidth 64) -> read64 br
        _ -> return Nothing

readName_ :: VTContext -> Address -> IO Text
readName_ ctx addr = do
  let readr = ctx ^. VTable.reader
  seekBinaryReader readr $ fromIntegral addr
  str <- unfoldWhileJustM (readAndReturnByteString readr)
  return $ decodeUtf8 $ BS.pack str
  where
    readAndReturnByteString :: BNBinaryReader -> IO (Maybe Word8)
    readAndReturnByteString br = do
      currentPosition <- getReaderPosition br
      char <- read8 br
      seekBinaryReader br $ fromIntegral (currentPosition + 1)
      case char of
        Just (0 :: Word8) -> return Nothing
        Nothing -> return Nothing
        Just p -> return $ Just p

getTypeInfo_ :: Address -> VTable.Ctx (Maybe VTable.TypeInfo)
getTypeInfo_ vptr = do
  ctx <- ask
  let readr = ctx ^. VTable.reader
  let (AddressWidth bitW) = ctx ^. VTable.width
  liftIO $ seekBinaryReader readr $ fromIntegral vptr - toBytes bitW
  ptrToTypeInfo <- case ctx ^. VTable.width of
    (AddressWidth 8) -> liftIO $ read64 (ctx ^. VTable.reader)
    _ -> return Nothing
  case ptrToTypeInfo of
    Nothing -> return Nothing
    Just p -> createTypeInfo_ (Address $ Bytes p)

getVirtualFunctions_ :: Address -> VTable.Ctx [Function]
getVirtualFunctions_ initVptr = do
  ctx <- ask
  let readr = ctx ^. VTable.reader
  liftIO $ seekBinaryReader readr $ fromIntegral initVptr
  fs <-
    liftIO $
      unfoldWhileJustM
        (getFunctionAndUpdateReader (ctx ^. VTable.bv) readr (ctx ^. VTable.width))
  liftIO $ traverse (convertFunction (ctx ^. VTable.bv)) fs
  where
    getFunctionAndUpdateReader :: BNBinaryView -> BNBinaryReader -> AddressWidth -> IO (Maybe BF.Function)
    getFunctionAndUpdateReader bv br width = do
      currentPosition <- getReaderPosition br
      fAddr <- case width of
        (AddressWidth 64) -> read64 br
        _ -> return Nothing
      let (AddressWidth bitW) = width
      seekBinaryReader br $ currentPosition + toBytes bitW
      maybe (return Nothing) (getFunctionStartingAt bv Nothing . Address . Bytes) fAddr

createVTable_ :: Address -> VTable.Ctx VTable.VTable
createVTable_ vptr = do
  topOffset <- getTopOffset_ vptr
  typeInfo <- getTypeInfo_ vptr
  vFunctions <- getVirtualFunctions_ vptr
  return
    ( VTable
        { _topOffset = topOffset,
          _typeInfo = typeInfo,
          _vptrAddress = vptr,
          _vFunctions = vFunctions,
          _parents = Nothing
        }
    )

initialVTContext_ :: BNBinaryView -> IO VTContext
initialVTContext_ bv = do
  width <- getAddressSize bv
  readr <- getDefaultReader bv
  return
    ( VTContext
        { _width = width,
          _reader = readr,
          _bv = bv
        }
    )

getVTable :: BNBinaryView -> Address -> IO VTable.VTable
getVTable bv addr = do
  initContext <- initialVTContext_ bv
  let vtable = createVTable_ addr
  runReaderT vtable initContext

-- | the 'getVTableStores' function checks for an array of function pointers, not a vtable
getVTableStores :: BN.BNBinaryView -> [Pil.Stmt] -> IO [(Pil.Stmt, Address)]
getVTableStores bv stmts = filterM (isVtable bv . snd) storeConst
  where
    storeConst =
      [ (storeStmt, Address $ fromIntegral vptrCandidate)
        | storeStmt@(Pil.Store (Pil.StoreOp _ (Pil.Expression _ (Pil.CONST (Pil.ConstOp vptrCandidate))))) <- stmts
      ]

isVtable :: BN.BNBinaryView -> Address -> IO Bool
isVtable bv addr = do
  readr <- getDefaultReader bv
  BN.seekBinaryReader readr $ fromIntegral addr
  getAddressSize bv >>= \case
    (AddressWidth 8) -> BN.read64 readr >>= \case
      Nothing -> return False
      Just ptr ->
        getSectionTypeOfAddr bv addr >>= \case
          "PROGBITS" ->
            isJust <$> (BF.getFunctionStartingAt bv Nothing ((Address . fromIntegral) ptr) :: IO (Maybe BF.Function))
          _ -> return False
    _ -> return False

getSectionTypeOfAddr :: BN.BNBinaryView -> Address -> IO Text
getSectionTypeOfAddr bv addr = pack <$> (BN.getSectionsAt bv addr >>= BN.getSectionType . head)

getVTables :: BNBinaryView -> [Address] -> IO [VTable]
getVTables bv = mapM (getVTable bv)
