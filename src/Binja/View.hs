module Binja.View where

import Binja.C.Main
  ( BNBinaryReader,
    BNBinaryView,
  )
import qualified Binja.C.Main as BN
import Binja.Prelude hiding (reader)

getDefaultReader :: BNBinaryView -> IO BNBinaryReader
getDefaultReader bv = do
  (Just reader) <- BN.createBinaryReader bv
  defaultEndianness <- BN.getDefaultEndianness bv
  BN.setBinaryReaderEndianness reader defaultEndianness
  return reader

readByte :: BNBinaryView -> Word64 -> IO Word8
readByte bv offset = do
  reader <- getDefaultReader bv
  BN.seekBinaryReader reader offset
  (Just val) <- BN.read8 reader
  return val

readBytes :: BNBinaryView -> Word64 -> Word64 -> IO [Word8]
readBytes bv offset numBytes = do
  reader <- getDefaultReader bv
  BN.seekBinaryReader reader offset
  (Just vals) <- BN.readData reader numBytes
  return vals