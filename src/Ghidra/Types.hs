{-# LANGUAGE DataKinds #-}
module Ghidra.Types where

import Ghidra.Prelude
import qualified Language.Java as Java
import Language.Java (J)
import qualified Foreign.JNI.Types as JNIT

-- newtype Address = Address (J ('Java.Class "ghidra.program.model.address.Address"))
-- newtype AddressSet = AddressSet (J ('Java.Class "ghidra.program.model.address.AddressSet"))

type Address = J ('Java.Class "ghidra.program.model.address.Address")
type AddressIterator = J ('Java.Class "ghidra.program.model.address.AddressIterator")
type AddressSet = J ('Java.Class "ghidra.program.model.address.AddressSetView")
type DecompInterface = J ('Java.Class "ghidra.app.decompiler.DecompInterface")
type DecompilerResults = J ('Java.Class "ghidra.app.decompiler.DecompileResults")
type FlatDecompilerAPI = J ('Java.Class "ghidra.app.decompiler.flatapi.FlatDecompilerAPI")
type Function = J ('Java.Class "ghidra.program.model.listing.Function")
type HighFunction = J ('Java.Class "ghidra.program.model.pcode.HighFunction")
type Instruction = J ('Java.Class "ghidra.program.model.listing.Instruction")
type InstructionIterator = J ('Java.Class "ghidra.program.model.listing.InstructionIterator")
type Iterator a = J ('Java.Class "java.util.Iterator")
type Listing = J ('Java.Class "ghidra.program.model.listing.Listing")
type PcodeOp = J ('Java.Class "ghidra.program.model.pcode.PcodeOp")
type PcodeOpAST = J ('Java.Class "ghidra.program.model.pcode.PcodeOpAST")
type TaskMonitor = J ('Java.Class "ghidra.util.task.TaskMonitor")
type VarNode = J ('Java.Class "ghidra.program.model.pcode.Varnode")
type VarNodeAST = J ('Java.Class "ghidra.program.model.pcode.VarnodeAST")


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

instance Addressable Instruction where
  toAddr x = Java.call x "getAddress"
  toAddrSet = Java.new <=< toAddr

instance Addressable Function where
  toAddr fn = Java.call fn "getEntryPoint"
  toAddrSet fn = Java.call fn "getBody"
