module Binja.View where

import Binja.C.Main
  ( BNBinaryReader,
    BNBinaryView
  )
import qualified Binja.C.Main as BN
import Binja.Types.StringReference (BNStringReference)
import qualified Binja.Types.StringReference as StrRef
import Binja.Prelude hiding (reader)
import Binja.C.Enums as BNEnums
import Binja.Types.Symbol (BNNameSpace)
import Binja.C.Pointers (BNSymbol)

import Data.BinaryAnalysis (Address)

import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HMap
import Data.HashMap.Strict (HashMap)
import Data.Text.Encoding as TE
import qualified Data.ByteString as BS

getDefaultReader :: BNBinaryView -> IO BNBinaryReader
getDefaultReader bv = do
  (Just reader) <- BN.createBinaryReader bv
  defaultEndianness <- BN.getDefaultEndianness bv
  BN.setBinaryReaderEndianness reader defaultEndianness
  return reader

readByte :: BNBinaryView -> Address -> IO Word8
readByte bv offset = do
  reader <- getDefaultReader bv
  BN.seekBinaryReader reader $ fromIntegral offset
  (Just val) <- BN.read8 reader
  return val

readBytes :: BNBinaryView -> Address -> Word64 -> IO [Word8]
readBytes bv offset numBytes = do
  reader <- getDefaultReader bv
  BN.seekBinaryReader reader $ fromIntegral offset
  (Just vals) <- BN.readData reader numBytes
  return vals

convertStringRef :: BNBinaryView -> BNStringReference -> IO Text
convertStringRef bv x =
  decode <$> bytes
  where
    decode :: ByteString -> Text
    decode bs =
      case x ^. StrRef.stringType of
        BNEnums.AsciiString -> TE.decodeUtf8 bs
        BNEnums.Utf8String -> TE.decodeUtf8 bs
        BNEnums.Utf16String -> TE.decodeUtf16LE bs
        BNEnums.Utf32String -> TE.decodeUtf32LE bs
    bytes :: IO ByteString
    bytes = BS.pack <$> readBytes bv (x ^. StrRef.start) (x ^. StrRef.length)

getStrings :: BNBinaryView -> IO [Text]
getStrings bv = do
  refs <- BN.getStringRefs bv
  traverse (convertStringRef bv) refs

getStringsMap :: BNBinaryView -> IO (HashMap Address Text)
getStringsMap bv = do
  refs <- BN.getStringRefs bv
  let addrs = fmap (fromIntegral . view StrRef.start) refs
  strs <- traverse (convertStringRef bv) refs
  return (HMap.fromList $ zip addrs strs)

-- TODO: Word64 should be Address, but don't want to leak that type from a Binja.C.* module
getStringAtAddress :: BNBinaryView -> Address -> IO (Maybe Text)
getStringAtAddress bv addr = do
  maybeRef <- BN.getStringRefAtAddress bv (fromIntegral addr)
  case maybeRef of
    (Just ref) -> Just <$> convertStringRef bv ref
    Nothing -> return Nothing


getSymbolAtAddress :: BNBinaryView -> Address -> Maybe BNNameSpace -> IO (Maybe BNSymbol)
getSymbolAtAddress bv addr =
  BN.getSymbolByAddress bv (coerce addr)
