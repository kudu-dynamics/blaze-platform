{-# LANGUAGE DataKinds #-}
module Ghidra.Types where

import Ghidra.Prelude
import qualified Language.Java as Java
import Language.Java (J)
import qualified Foreign.JNI.Types as JNIT

-- newtype Address = Address (J ('Java.Class "ghidra.program.model.address.Address"))
-- newtype AddressSet = AddressSet (J ('Java.Class "ghidra.program.model.address.AddressSet"))

type Address = J ('Java.Class "ghidra.program.model.address.Address")
type AddressSet = J ('Java.Class "ghidra.program.model.address.AddressSetView")
type AddressIterator = J ('Java.Class "ghidra.program.model.address.AddressIterator")
type Listing = J ('Java.Class "ghidra.program.model.listing.Listing")
type FlatDecompilerAPI = J ('Java.Class "ghidra.app.decompiler.flatapi.FlatDecompilerAPI")
type DecompInterface = J ('Java.Class "ghidra.app.decompiler.DecompInterface")
type TaskMonitor = J ('Java.Class "ghidra.util.task.TaskMonitor")
type DecompilerResults = J ('Java.Class "ghidra.app.decompiler.DecompileResults")
type Iterator a = J ('Java.Class "java.util.Iterator")


class Addressable a where
  toAddr :: a -> IO Address
  toAddrSet :: a -> IO AddressSet

instance Addressable Address where
  toAddr = pure
  toAddrSet a = Java.new a

addressIteratorToList :: AddressIterator -> IO [Address]
addressIteratorToList x = do
  hasNext :: Bool <- Java.call x "hasNext"
  if hasNext
    then do
      addr <- Java.call x "next"
      (addr:) <$> addressIteratorToList x
    else return []

toAddrs :: Addressable a => a -> IO [Address]
toAddrs x = do
  s <- toAddrSet x
  Java.call s "getAddresses" True >>= addressIteratorToList

isJNull :: J a -> Bool
isJNull x = x == JNIT.jnull

maybeNull :: J a -> Maybe (J a)
maybeNull x = bool Nothing (Just x) $ isJNull x
