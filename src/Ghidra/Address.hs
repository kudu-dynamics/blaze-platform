{-# LANGUAGE DataKinds #-}
module Ghidra.Address
  ( module Ghidra.Address
  , AddressSpace
  , Address
  ) where

import Ghidra.Prelude hiding (toList)

import qualified Language.Java as Java
import qualified Ghidra.Types as J
import Ghidra.Types.Address (AddressSpace(AddressSpace), Address(Address), AddressSpaceId(AddressSpaceId))
import qualified Ghidra.State as State
import Ghidra.State (GhidraState)
import qualified Data.HashMap.Strict as HashMap
import qualified Ghidra.Program as Program


mkAddressSpace :: J.AddressSpace -> IO AddressSpace
mkAddressSpace x = do
  ptrSize :: Int32 <- Java.call x "getPointerSize"
  addressableUnitSize :: Int32 <- Java.call x "getAddressableUnitSize"
  name :: Text <- Java.call x "getName" >>= Java.reify
  return $ AddressSpace (fromIntegral ptrSize) (fromIntegral addressableUnitSize) name

mkAddress :: J.Address -> IO Address
mkAddress addr = do
  addrSpace <- Java.call addr "getAddressSpace" >>= mkAddressSpace
  offset :: Int64 <- Java.call addr "getOffset"
  return $ Address addrSpace (fromIntegral offset)

getAddressSpaces :: GhidraState -> IO (HashMap AddressSpaceId AddressSpace)
getAddressSpaces gs = do
  af <- State.getProgram gs >>= Program.getAddressFactory
  spaces :: [J.AddressSpace] <- Java.call af "getAllAddressSpaces" >>= Java.reify
  HashMap.fromList <$> traverse f spaces
  where
    f :: J.AddressSpace -> IO (AddressSpaceId, AddressSpace)
    f x = do
      id <- AddressSpaceId <$> Java.call x "getSpaceID"
      (id,) <$> mkAddressSpace x
