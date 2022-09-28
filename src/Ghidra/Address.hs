{-# LANGUAGE DataKinds #-}
module Ghidra.Address
  ( module Ghidra.Address
  , AddressSpace
  , Address
  , AddressSpaceMap
  , AddressSpaceId
  ) where

import Ghidra.Prelude hiding (toList)

import qualified Language.Java as Java
import qualified Ghidra.Types as J
import Ghidra.Types.Address (AddressSpace(AddressSpace), Address(Address), AddressSpaceId(AddressSpaceId), AddressSpaceMap, AddressSpaceName)
import qualified Ghidra.Types.Address as Addr
import qualified Ghidra.State as State
import Ghidra.State (GhidraState)
import qualified Data.HashMap.Strict as HashMap
import qualified Ghidra.Program as Program
import qualified Foreign.JNI as JNI


readAddressSpaceName :: Text -> AddressSpaceName
readAddressSpaceName t = case t of
  "EXTERNAL" -> Addr.EXTERNAL
  "HASH" -> Addr.HASH
  "const" -> Addr.Const
  "ram" -> Addr.Ram
  "register" -> Addr.Register
  "stack" -> Addr.Stack
  "unique" -> Addr.Unique
  _ -> Addr.Other t

mkAddressSpace :: J.AddressSpace -> IO AddressSpace
mkAddressSpace x = do
  ptrSize :: Int32 <- Java.call x "getPointerSize"
  addressableUnitSize :: Int32 <- Java.call x "getAddressableUnitSize"
  name :: Text <- Java.call x "getName" >>= JNI.newGlobalRef >>= Java.reify
  return $ AddressSpace (fromIntegral ptrSize) (fromIntegral addressableUnitSize) (readAddressSpaceName name)

mkAddress :: J.Address -> IO Address
mkAddress addr = do
  addrSpace <- Java.call addr "getAddressSpace" >>= mkAddressSpace
  offset :: Int64 <- Java.call addr "getOffset"
  return $ Address addrSpace (fromIntegral offset)

getAddressSpaceMap :: GhidraState -> IO AddressSpaceMap
getAddressSpaceMap gs = do
  af <- State.getProgram gs >>= Program.getAddressFactory
  spaces :: [J.AddressSpace] <- Java.call af "getAllAddressSpaces"
                                >>= Java.reify
                                >>= traverse JNI.newGlobalRef
  HashMap.fromList <$> traverse f spaces
  where
    f :: J.AddressSpace -> IO (AddressSpaceId, AddressSpace)
    f x = do
      id <- AddressSpaceId <$> Java.call x "getSpaceID"
      (id,) <$> mkAddressSpace x
