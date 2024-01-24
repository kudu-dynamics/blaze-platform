{-# LANGUAGE DataKinds #-}
module Ghidra.Address
  ( module Ghidra.Address
  , module Exports
  ) where

import Ghidra.Prelude hiding (toList, Const)

import qualified Ghidra.Types as J
import Ghidra.Types.Address as Exports
import Ghidra.Types.Internal (Ghidra, runIO)

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

mkAddressSpace :: J.AddressSpace -> Ghidra AddressSpace
mkAddressSpace x = runIO $ do
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

--- | Returns the register `AddressSpace`.
getRegisterSpace :: J.AddressFactory -> Ghidra AddressSpace
getRegisterSpace af = do
  space :: J.AddressSpace <- runIO $ Java.call af "getRegisterSpace"
                             >>= JNI.newGlobalRef
  mkAddressSpace space

mkAddress :: J.Address -> Ghidra Address
mkAddress addr = do
  addrSpace <- runIO (Java.call addr "getAddressSpace") >>= mkAddressSpace
  offset :: Int64 <- runIO $ Java.call addr "getOffset"
  return $ Address addrSpace (fromIntegral offset)

getAddress :: J.AddressFactory -> Int32 -> Int64 -> Ghidra J.Address
getAddress af space offset = runIO $ 
  Java.call af "getAddress" space offset
  >>= JNI.newGlobalRef
  >>= Java.reify
