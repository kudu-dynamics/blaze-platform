module Blaze.VTable where

import qualified Binja.Core as BN
import Binja.Core (BNBinaryReader, BNBinaryView, getReaderPosition, getViewAddressSize, read64, read8, seekBinaryReader)
import qualified Binja.Function as BF
import Binja.Function (getFunctionStartingAt)
import qualified Binja.View as BV
import Binja.View (getDefaultReader)
import Blaze.CallGraph (Function)
import Blaze.Prelude
import Blaze.Types.Import.BinaryNinja (convertFunction)
import qualified Blaze.Types.Pil as Pil
import qualified Blaze.Types.VTable as VTable
import Blaze.Types.VTable
  ( TypeInfo (TypeInfo, _helperClass, _name, _parentsTypeInfo),
    VTContext (VTContext, _bv, _reader, _width),
    VTable (VTable, _parents, _topOffset, _typeInfo, _vFunctions, _vptrAddress),
  )
import Data.BinaryAnalysis (Address (Address))
import Data.Text (pack)
import qualified Data.ByteString as BS

getTopOffset_ :: Address -> VTable.Ctx (Maybe Word64)
getTopOffset_ vptr = do
  ctx <- ask
  _ <- liftIO $ seekBinaryReader (ctx ^. VTable.reader) (fromIntegral $ vptr - toAddress (2 * ctx ^. VTable.width))
  case ctx ^. VTable.width of
    8 -> liftIO $ read64 (ctx ^. VTable.reader)
    _ -> return Nothing
  where
    toAddress = Address . fromIntegral

createTypeInfo_ :: Word64 -> VTable.Ctx (Maybe VTable.TypeInfo)
createTypeInfo_ typeInfoPtr
  | typeInfoPtr == 0 = return Nothing
  | otherwise = do
    ctx <- ask
    helperClassPtr <- liftIO $ seekAndRead (ctx ^. VTable.reader) (ctx ^. VTable.width) typeInfoPtr >>= \case
      Nothing -> return Nothing
      Just p -> return $ Just $ (Address . fromIntegral) p
    name <- liftIO $ seekAndRead (ctx ^. VTable.reader) (ctx ^. VTable.width) (typeInfoPtr + ctx ^. VTable.width) >>= \case
      Nothing -> return Nothing
      Just p -> readName_ ctx $ (Address . fromIntegral) p
    parentTypeInfoPtr <- liftIO $ seekAndRead (ctx ^. VTable.reader) (ctx ^. VTable.width) (typeInfoPtr + 2 * ctx ^. VTable.width)
    parentTypeInfo <- case parentTypeInfoPtr of
      Nothing -> return Nothing
      Just p -> createTypeInfo_ p
    return $
      Just
        ( TypeInfo
            { _helperClass = helperClassPtr,
              _name = name,
              _parentsTypeInfo = parentTypeInfo
            }
        )
  where
    seekAndRead :: BNBinaryReader -> Word64 -> Word64 -> IO (Maybe Word64)
    seekAndRead br width addr = do
      _ <- seekBinaryReader br addr
      case width of
        8 -> read64 br
        _ -> return Nothing

readName_ :: VTContext -> Address -> IO (Maybe Text)
readName_ ctx addr = do
  let readr = ctx ^. VTable.reader
  _ <- liftIO $ seekBinaryReader readr $ fromIntegral addr
  str <-
    liftIO $
      takeWhileM_
        (/= Just 0)
        (readAndReturnByteString readr)
  return $ Just $ decodeUtf8 $ BS.pack $ fromJust <$> str
  where
    readAndReturnByteString :: BNBinaryReader -> IO (Maybe Word8)
    readAndReturnByteString br = do
      currentPosition <- getReaderPosition br
      char <- read8 br
      _ <- seekBinaryReader br $ fromIntegral (currentPosition + 1)
      return char


getTypeInfo_ :: Address -> VTable.Ctx (Maybe VTable.TypeInfo)
getTypeInfo_ vptr = do
  ctx <- ask
  let readr = ctx ^. VTable.reader
  _ <- liftIO $ seekBinaryReader readr $ fromIntegral (vptr - toAddress (ctx ^. VTable.width))
  ptrToTypeInfo <- case ctx ^. VTable.width of
    8 -> liftIO $ read64 (ctx ^. VTable.reader)
    _ -> return Nothing
  case ptrToTypeInfo of
    Nothing -> return Nothing
    Just p -> liftIO $ createTypeInfo_ p
  where
    toAddress = Address . fromIntegral

getVirtualFunctions_ :: Address -> VTable.Ctx [Function]
getVirtualFunctions_ initVptr = do
  ctx <- ask
  let readr = ctx ^. VTable.reader
  _ <- liftIO $ seekBinaryReader readr $ fromIntegral initVptr
  fs <-
    liftIO $
      takeWhileM_
        (/= Nothing)
        (getFunctionAndUpdateReader (ctx ^. VTable.bv) readr (ctx ^. VTable.width))
  let tmp = fromJust <$> fs
  liftIO $ traverse (convertFunction (ctx ^. VTable.bv)) tmp
  where
    getFunctionAndUpdateReader :: BNBinaryView -> BNBinaryReader -> Word64 -> IO (Maybe BF.Function)
    getFunctionAndUpdateReader bv br width = do
      currentPosition <- getReaderPosition br
      fAddr <- case width of
        8 -> read64 br
        _ -> return Nothing
      _ <- seekBinaryReader br $ fromIntegral (currentPosition + width)
      getFunctionStartingAt bv Nothing $ (Address . fromJust) fAddr

takeWhileM_ :: (a -> Bool) -> IO a -> IO [a]
takeWhileM_ p act = do
  x <- act
  if p x
    then (x :) <$> takeWhileM_ p act
    else return []

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
  width <- getViewAddressSize bv
  readr <- getDefaultReader bv
  return
    ( VTContext
        { _width = fromIntegral width,
          _reader = readr,
          _bv = bv
        }
    )

getVTable :: BNBinaryView -> Address -> IO VTable.VTable
getVTable bv addr = do
  initContext <- initialVTContext_ bv
  let vtable = createVTable_ addr
  runReaderT vtable initContext

-- currently this just checks for an array of function pointers, not a vtable
getVtableStores :: BN.BNBinaryView -> [Pil.Stmt] -> IO [Pil.Stmt]
getVtableStores bv stmts = map fst <$> filterM (isVtable bv . snd) storeConst
  where
    storeConst =
      [ (storeStmt, Address <$> toIntegralSized vptrCandidate)
        | storeStmt@(Pil.Store (Pil.StoreOp _ (Pil.Expression _ (Pil.CONST (Pil.ConstOp vptrCandidate))))) <- stmts
      ]

isVtable :: BN.BNBinaryView -> Maybe Address -> IO Bool
isVtable _ Nothing =
  return False
isVtable bv mAddr = do
  let addr = fromJust mAddr
  readr <- BV.getDefaultReader bv
  _ <- BN.seekBinaryReader readr $ fromIntegral addr
  addrSize <- BN.getViewAddressSize bv
  val <- case addrSize of
    8 -> BN.read64 readr
    _ -> return Nothing
  case val of
    Nothing -> return False
    _ -> do
      sectionType <- getSectionTypeOfAddr bv addr
      case sectionType of
        "PROGBITS" ->
          isJust <$> BF.getFunctionStartingAt bv Nothing (toAddress val)
        _ -> return False
  where
    toAddress :: Maybe Word64 -> Address
    toAddress = Address . fromJust . toIntegralSized . fromJust

getSectionTypeOfAddr :: BN.BNBinaryView -> Address -> IO Text
getSectionTypeOfAddr bv addr = pack <$> (BN.getSectionsAt bv addr >>= BN.getSectionType . head)
