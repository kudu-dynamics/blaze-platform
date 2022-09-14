{-# LANGUAGE DataKinds #-}
module Ghidra.Types where

import Ghidra.Prelude
import qualified Language.Java as Java
import Language.Java (J)
import qualified Foreign.JNI.Types as JNIT

-- newtype Address = Address (J ('Java.Class "ghidra.program.model.address.Address"))
-- newtype AddressSet = AddressSet (J ('Java.Class "ghidra.program.model.address.AddressSet"))

type Address = J ('Java.Class "ghidra.program.model.address.Address")
type AddressFactory = J ('Java.Class "ghidra.program.model.address.AddressFactory")
type AddressIterator = J ('Java.Class "ghidra.program.model.address.AddressIterator")
type AddressSpace = J ('Java.Class "ghidra.program.model.address.AddressSpace")
type AddressSet = J ('Java.Class "ghidra.program.model.address.AddressSetView")
type AutoImporter = J ('Java.Class "ghidra.app.util.importer.AutoImporter")
type BasicBlockModel = J ('Java.Class "ghidra.program.model.block.BasicBlockModel")
type Class = J ('Java.Class "java.lang.Class")
type CodeBlock = J ('Java.Class "ghidra.program.model.block.CodeBlock")
type CodeBlockIterator = J ('Java.Class "ghidra.program.model.block.CodeBlockIterator")
type CodeBlockReferenceIterator = J ('Java.Class "ghidra.program.model.block.CodeBlockReferenceIterator")
type CodeBlockReference = J ('Java.Class "ghidra.program.model.block.CodeBlockReference")
type CompilerSpec = J ('Java.Class "ghidra.program.model.lang.CompilerSpec")
type CompilerSpecID = J ('Java.Class "ghidra.program.model.lang.CompilerSpecID")
type DataType = J ('Java.Class "ghidra.program.model.data.DataType")
type DecompInterface = J ('Java.Class "ghidra.app.decompiler.DecompInterface")
type DecompilerResults = J ('Java.Class "ghidra.app.decompiler.DecompileResults")
type File = J ('Java.Class "java.io.File")
type FlatDecompilerAPI = J ('Java.Class "ghidra.app.decompiler.flatapi.FlatDecompilerAPI")
type FlatProgramAPI = J ('Java.Class "ghidra.program.flatapi.FlatProgramAPI")
type Function = J ('Java.Class "ghidra.program.model.listing.Function")
type FunctionIterator = J ('Java.Class "ghidra.program.model.listing.FunctionIterator")
type GhidraJarApplicationLayout = J ('Java.Class "ghidra.GhidraJarApplicationLayout")
type HeadlessGhidraApplicationConfiguration = J ('Java.Class "ghidra.framework.HeadlessGhidraApplicationConfiguration")
type HighFunction = J ('Java.Class "ghidra.program.model.pcode.HighFunction")
type HighVariable = J ('Java.Class "ghidra.program.model.pcode.HighVariable")
type HighConstant = J ('Java.Class "ghidra.program.model.pcode.HighConstant")
type HighGlobal = J ('Java.Class "ghidra.program.model.pcode.HighGlobal")
type HighLocal = J ('Java.Class "ghidra.program.model.pcode.HighLocal")
type HighOther = J ('Java.Class "ghidra.program.model.pcode.HighOther")
type HighParam = J ('Java.Class "ghidra.program.model.pcode.HighParam")
type HighSymbol = J ('Java.Class "ghidra.program.model.pcode.HighSymbol")
type Instruction = J ('Java.Class "ghidra.program.model.listing.Instruction")
type InstructionIterator = J ('Java.Class "ghidra.program.model.listing.InstructionIterator")
type Iterator a = J ('Java.Class "java.util.Iterator")
type Language = J ('Java.Class "ghidra.program.model.lang.Language")
type LanguageID = J ('Java.Class "ghidra.program.model.lang.LanguageID")
type Listing = J ('Java.Class "ghidra.program.model.listing.Listing")
type MessageLog = J ('Java.Class "ghidra.app.util.importer.MessageLog")
type Object = J ('Java.Class "java.lang.Object")
type OutputStream = J ('Java.Class "java.io.OutputStream")
type PcodeOp = J ('Java.Class "ghidra.program.model.pcode.PcodeOp")
type PcodeOpAST = J ('Java.Class "ghidra.program.model.pcode.PcodeOpAST")
type PrintStream = J ('Java.Class "java.io.PrintStream")
type Program = J ('Java.Class "ghidra.program.database.Program")
type ProgramDB = J ('Java.Class "ghidra.program.database.ProgramDB")
type Scalar = J ('Java.Class "ghidra.program.model.scalar.Scalar")
type SimpleBlockModel = J ('Java.Class "ghidra.program.model.block.SimpleBlockModel")
type SleighLanguageProvider = J ('Java.Class "ghidra.app.plugin.processors.sleigh.SleighLanguageProvider")

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

instance Addressable CodeBlock where
  toAddr block = Java.call block "getFirstStartAddress"
  toAddrSet = return . coerce

-- | Equality check that extends beyond pointer location of J objects
-- This is especially necessary because we use newGlobalRef so often.
class JEqual a where
  (<==>) :: a -> a -> IO Bool

instance JEqual Address where
  a <==> b = (==) <$> f a <*> f b
    where
      f addr = do
        addrSpace :: AddressSpace <- Java.call addr "getAddressSpace"
        id :: Int32 <- Java.call addrSpace "getSpaceID"
        offset :: Int64 <- Java.call addr "getOffset"
        return (id, offset)

jEqualAddressable :: Addressable a => a -> a -> IO Bool
jEqualAddressable a b = ((,) <$> toAddr a <*> toAddr b) >>= uncurry (<==>)

instance JEqual Function where
  (<==>) = jEqualAddressable

instance JEqual CodeBlock where
  (<==>) = jEqualAddressable
