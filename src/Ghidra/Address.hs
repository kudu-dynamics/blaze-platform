{-# LANGUAGE DataKinds #-}
module Ghidra.Address
  ( module Ghidra.Address
  , module Exports
  ) where

import Ghidra.Prelude hiding (toList, Const)

import qualified Ghidra.Types as J
import Ghidra.Types.Address as Exports

import qualified Language.Java as Java
import qualified Foreign.JNI as JNI

readAddressSpaceName :: Text -> AddressSpaceName
readAddressSpaceName t = case t of
  "EXTERNAL" -> EXTERNAL
  "HASH" -> HASH
  "const" -> Const
  "ram" -> Ram
  "register" -> Register
  "stack" -> Stack
  "unique" -> Unique
  _ -> Other t

mkAddressSpace :: J.AddressSpace -> IO AddressSpace
mkAddressSpace x = do
  spaceId :: Int32 <- Java.call x "getSpaceID"
  ptrSize :: Int32 <- Java.call x "getPointerSize"
  addressableUnitSize :: Int32 <- Java.call x "getAddressableUnitSize"
  name :: Text <- Java.call x "getName" >>= JNI.newGlobalRef >>= Java.reify
  return $
    AddressSpace
      (fromIntegral spaceId)
      (fromIntegral ptrSize)
      (fromIntegral addressableUnitSize)
      (readAddressSpaceName name)

mkAddress :: J.Address -> IO Address
mkAddress addr = do
  addrSpace <- Java.call addr "getAddressSpace" >>= mkAddressSpace
  offset :: Int64 <- Java.call addr "getOffset"
  return $ Address addrSpace (fromIntegral offset)

getAddress :: J.AddressFactory -> Int32 -> Int64 -> IO J.Address
getAddress af space offset =
  Java.call af "getAddress" space offset
  >>= JNI.newGlobalRef
  >>= Java.reify

-- | Returns the register `AddressSpace`.
getRegisterSpace :: J.AddressFactory -> IO AddressSpace
getRegisterSpace af = do
  space :: J.AddressSpace <- Java.call af "getRegisterSpace"
                             >>= JNI.newGlobalRef
  mkAddressSpace space
