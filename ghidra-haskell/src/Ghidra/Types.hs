{-# LANGUAGE DataKinds #-}
module Ghidra.Types
  ( module Ghidra.Types
  , module Exports
  ) where

import Ghidra.Prelude hiding (String)
import Ghidra.Types.Internal as Exports (Ghidra)
import Ghidra.Types.Internal (runIO)
import qualified Language.Java as Java
import qualified Foreign.JNI as JNI
import Language.Java (J)


type Address = J ('Java.Class "ghidra.program.model.address.Address")
type AddressFactory = J ('Java.Class "ghidra.program.model.address.AddressFactory")
type AddressIterator = J ('Java.Class "ghidra.program.model.address.AddressIterator")
type AddressSet = J ('Java.Class "ghidra.program.model.address.AddressSet")
type AddressSetView = J ('Java.Class "ghidra.program.model.address.AddressSetView")
type AddressSpace = J ('Java.Class "ghidra.program.model.address.AddressSpace")
type ApplicationConfiguration = J ('Java.Class "ghidra.framework.ApplicationConfiguration")
type ApplicationLayout = J ('Java.Class "utility.application.ApplicationLayout")
type ArrayList a = J ('Java.Class "java.util.ArrayList")
type AutoImporter = J ('Java.Class "ghidra.app.util.importer.AutoImporter")
type BasicBlockModel = J ('Java.Class "ghidra.program.model.block.BasicBlockModel")
type Class = J ('Java.Class "java.lang.Class")
type CodeBlock = J ('Java.Class "ghidra.program.model.block.CodeBlock")
type CodeBlockIterator = J ('Java.Class "ghidra.program.model.block.CodeBlockIterator")
type CodeBlockReference = J ('Java.Class "ghidra.program.model.block.CodeBlockReference")
type CodeBlockReferenceIterator = J ('Java.Class "ghidra.program.model.block.CodeBlockReferenceIterator")
type CompilerSpec = J ('Java.Class "ghidra.program.model.lang.CompilerSpec")
type CompilerSpecID = J ('Java.Class "ghidra.program.model.lang.CompilerSpecID")
type DataType = J ('Java.Class "ghidra.program.model.data.DataType")
type DecompInterface = J ('Java.Class "ghidra.app.decompiler.DecompInterface")
type DecompilerResults = J ('Java.Class "ghidra.app.decompiler.DecompileResults")
type DomainFolder = J ('Java.Class "ghidra.framework.model.DomainFolder")
type DomainObject = J ('Java.Class "ghidra.framework.model.DomainObject")
type DomainObjectAdapterDB = J ('Java.Class "ghidra.framework.data.DomainObjectAdapterDB")
type ExternalLocationDB = J ('Java.Class "ghidra.program.database.external.ExternalLocationDB")
type ExternalLocation = J ('Java.Class "ghidra.program.model.symbol.ExternalLocation")
type File = J ('Java.Class "java.io.File")
type FlatDecompilerAPI = J ('Java.Class "ghidra.app.decompiler.flatapi.FlatDecompilerAPI")
type FlatProgramAPI = J ('Java.Class "ghidra.program.flatapi.FlatProgramAPI")
type Function = J ('Java.Class "ghidra.program.model.listing.Function")
type FunctionIterator = J ('Java.Class "ghidra.program.model.listing.FunctionIterator")
type GhidraJarApplicationLayout = J ('Java.Class "ghidra.GhidraJarApplicationLayout")
type HeadlessGhidraApplicationConfiguration = J ('Java.Class "ghidra.framework.HeadlessGhidraApplicationConfiguration")
type HighConstant = J ('Java.Class "ghidra.program.model.pcode.HighConstant")
type HighFunction = J ('Java.Class "ghidra.program.model.pcode.HighFunction")
type HighGlobal = J ('Java.Class "ghidra.program.model.pcode.HighGlobal")
type HighLocal = J ('Java.Class "ghidra.program.model.pcode.HighLocal")
type HighOther = J ('Java.Class "ghidra.program.model.pcode.HighOther")
type HighParam = J ('Java.Class "ghidra.program.model.pcode.HighParam")
type HighSymbol = J ('Java.Class "ghidra.program.model.pcode.HighSymbol")
type HighVariable = J ('Java.Class "ghidra.program.model.pcode.HighVariable")
type Instruction = J ('Java.Class "ghidra.program.model.listing.Instruction")
type InstructionDB = J ('Java.Class "ghidra.program.database.code.InstructionDB")
type InstructionIterator = J ('Java.Class "ghidra.program.model.listing.InstructionIterator")
type Iterator a = J ('Java.Class "java.util.Iterator")
type Language = J ('Java.Class "ghidra.program.model.lang.Language")
type LanguageID = J ('Java.Class "ghidra.program.model.lang.LanguageID")
type Listing = J ('Java.Class "ghidra.program.model.listing.Listing")
type Loaded a = J ('Java.Class "ghidra.app.util.opinion.Loaded")
type LoadResults a = J ('Java.Class "ghidra.app.util.opinion.LoadResults")
type MessageLog = J ('Java.Class "ghidra.app.util.importer.MessageLog")
type Object = J ('Java.Class "java.lang.Object")
type Options = J ('Java.Class "ghidra.framework.options.Options")
type OutputStream = J ('Java.Class "java.io.OutputStream")
type Parameter = J ('Java.Class "ghidra.program.model.listing.Parameter")
type PcodeBlock = J ('Java.Class "ghidra.program.model.pcode.PcodeBlock")
type PcodeBlockBasic = J ('Java.Class "ghidra.program.model.pcode.PcodeBlockBasic")

type PcodeOp = J ('Java.Class "ghidra.program.model.pcode.PcodeOp")
type PcodeOpAST = J ('Java.Class "ghidra.program.model.pcode.PcodeOpAST")
type PcodeSyntaxTree = J ('Java.Class "ghidra.program.model.pcode.PcodeSyntaxTree")

type PrintStream = J ('Java.Class "java.io.PrintStream")
type Program = J ('Java.Class "ghidra.program.model.listing.Program")
type ProgramDB = J ('Java.Class "ghidra.program.database.ProgramDB")
type Project = J ('Java.Class "ghidra.framework.model.Project")
type Reference = J ('Java.Class "ghidra.program.model.symbol.Reference")
type ReferenceIterator = J ('Java.Class "ghidra.program.model.symbol.ReferenceIterator")
type ReferenceManager = J ('Java.Class "ghidra.program.model.symbol.ReferenceManager")
type Register = J ('Java.Class "ghidra.program.model.lang.Register")
type Scalar = J ('Java.Class "ghidra.program.model.scalar.Scalar")
type SequenceNumber = J ('Java.Class "ghidra.program.model.pcode.SequenceNumber")
type SimpleBlockModel = J ('Java.Class "ghidra.program.model.block.SimpleBlockModel")
type SleighLanguageProvider = J ('Java.Class "ghidra.app.plugin.processors.sleigh.SleighLanguageProvider")
type String = J ('Java.Class "java.lang.String")
type TaskMonitor = J ('Java.Class "ghidra.util.task.TaskMonitor")
type VarNode = J ('Java.Class "ghidra.program.model.pcode.Varnode")
type VarNodeAST = J ('Java.Class "ghidra.program.model.pcode.VarnodeAST")


class Addressable a where
  toAddr :: a -> Ghidra Address
  toAddrSet :: a -> Ghidra AddressSet

instance Addressable Address where
  toAddr = pure
  toAddrSet = runIO . Java.new

addressIteratorToList :: AddressIterator -> Ghidra [Address]
addressIteratorToList x = do
  hasNext :: Bool <- runIO $ Java.call x "hasNext"
  if hasNext
    then do
      addr <- runIO $ Java.call x "next" >>= JNI.newGlobalRef
      (addr:) <$> addressIteratorToList x
    else return []

toAddrs :: Addressable a => a -> Ghidra [Address]
toAddrs x = do
  s <- toAddrSet x
  runIO (Java.call s "getAddresses" True) >>= addressIteratorToList

instance Addressable Instruction where
  toAddr x = runIO $ Java.call x "getAddress"
  toAddrSet = (runIO . Java.new) <=< toAddr

instance Addressable Function where
  toAddr fn = runIO $ Java.call fn "getEntryPoint"
  toAddrSet fn = do
    r :: AddressSetView <- runIO $ Java.call fn "getBody"
    return $ coerce r

instance Addressable CodeBlock where
  toAddr block = runIO $ Java.call block "getFirstStartAddress"
  toAddrSet = return . coerce

instance Addressable PcodeBlockBasic where
  toAddr pb = runIO $ Java.call pb "getStart"
  toAddrSet pb = runIO $ do
    start :: Address <- Java.call pb "getStart"
    end :: Address <- Java.call pb "getStop"
    Java.new start end

instance Addressable AddressSet where
  toAddr x = runIO $ Java.call x "getMinAddress"
  toAddrSet = return

mkAddressSetFromRange :: Address -> Address -> Ghidra AddressSet
mkAddressSetFromRange a = runIO . Java.new a

-- | Equality check that extends beyond pointer location of J objects
-- This is especially necessary because we use newGlobalRef so often.
class JEqual a where
  (<==>) :: a -> a -> Ghidra Bool

instance JEqual Address where
  a <==> b = (==) <$> f a <*> f b
    where
      f addr = runIO $ do
        addrSpace :: AddressSpace <- Java.call addr "getAddressSpace"
        id :: Int32 <- Java.call addrSpace "getSpaceID"
        offset :: Int64 <- Java.call addr "getOffset"
        return (id, offset)

jEqualAddressable :: Addressable a => a -> a -> Ghidra Bool
jEqualAddressable a b = ((,) <$> toAddr a <*> toAddr b) >>= uncurry (<==>)

instance JEqual Function where
  (<==>) = jEqualAddressable

instance JEqual CodeBlock where
  (<==>) = jEqualAddressable

arrayListToList :: (Coercible Object a) => ArrayList a -> Ghidra [a]
arrayListToList al = do
  xs :: [Object] <- runIO $ Java.call al "toArray" >>= Java.reify
  return $ coerce <$> xs
