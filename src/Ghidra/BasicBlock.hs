{-# LANGUAGE DataKinds #-}
module Ghidra.BasicBlock
  ( module Ghidra.BasicBlock
  ) where

import Ghidra.Prelude hiding (toList)

import Ghidra.State (GhidraState)
import qualified Ghidra.State as State
import Ghidra.Types.BasicBlock (BasicBlock(BasicBlock), BasicBlockGraph(BasicBlockGraph))
import Ghidra.Types.Internal (runIO, Ghidra)
import qualified Language.Java as Java
import qualified Ghidra.Address as Addr
import qualified Foreign.JNI as JNI
import qualified Ghidra.Types as J
import Ghidra.Types.Address (Address)
import qualified Data.Set as Set


codeBlockIteratorToList :: J.CodeBlockIterator -> Ghidra [J.CodeBlock]
codeBlockIteratorToList x = do
  hasNext :: Bool <- runIO $ Java.call x "hasNext"
  if hasNext
    then do
      ref <- runIO $ Java.call x "next" >>= JNI.newGlobalRef
      (ref:) <$> codeBlockIteratorToList x
    else return []

codeBlockReferenceIteratorToList :: J.CodeBlockReferenceIterator -> Ghidra [J.CodeBlockReference]
codeBlockReferenceIteratorToList x = do
  hasNext :: Bool <- runIO $ Java.call x "hasNext"
  if hasNext
    then do
      ref <- runIO $ Java.call x "next" >>= JNI.newGlobalRef
      (ref:) <$> codeBlockReferenceIteratorToList x
    else return []

getCodeBlocks :: J.Addressable a => GhidraState -> a -> Ghidra [J.CodeBlock]
getCodeBlocks gs x = do
  prg <- State.getProgram gs
  bbModel :: J.BasicBlockModel <- runIO $ Java.new (coerce prg :: J.Program)
  
  tm <- State.getTaskMonitor gs
  addrSet :: J.AddressSetView <- coerce $ J.toAddrSet x
  runIO (Java.call bbModel "getCodeBlocksContaining" addrSet tm) >>= codeBlockIteratorToList

getDestinations :: GhidraState -> J.CodeBlock -> Ghidra [J.CodeBlock]
getDestinations gs block = do
  tm <- State.getTaskMonitor gs
  blocks <- runIO (Java.call block "getDestinations" tm) >>= codeBlockReferenceIteratorToList
  mapM (runIO . JNI.newGlobalRef <=< runIO . (`Java.call` "getDestinationBlock")) blocks

getSources :: GhidraState -> J.CodeBlock -> Ghidra [J.CodeBlock]
getSources gs block = do
  tm <- State.getTaskMonitor gs
  blocks <- runIO (Java.call block "getSources" tm) >>= codeBlockReferenceIteratorToList
  mapM (runIO . JNI.newGlobalRef <=< runIO . (`Java.call` "getSourceBlock")) blocks

getStartAddress :: J.CodeBlock -> Ghidra Address
getStartAddress block
  = runIO (Java.call block "getFirstStartAddress") >>= Addr.mkAddress

getFunction :: GhidraState -> J.CodeBlock -> Ghidra J.Function
getFunction gs block = do
  prg <- State.getProgram gs
  listing :: J.Listing <- runIO $ Java.call prg "getListing"
  blockAddr <- J.toAddr block
  runIO $ Java.call listing "getFunctionContaining" blockAddr

shareFunc :: GhidraState -> J.CodeBlock -> J.CodeBlock -> Ghidra Bool
shareFunc gs a b = do
  funcA <- getFunction gs a
  funcB <- getFunction gs b
  return $ funcA == funcB

mkBasicBlock :: J.CodeBlock -> Ghidra BasicBlock
mkBasicBlock block = BasicBlock block <$> getStartAddress block
  
getEdgesForBlock
  :: GhidraState
  -> Set BasicBlock
  -> BasicBlock
  -> Ghidra [(BasicBlock, BasicBlock)]
getEdgesForBlock gs validDests srcBlock  = do
  destBlocks :: [BasicBlock] <- getDestinations gs (srcBlock ^. #handle) >>= traverse mkBasicBlock
  let onlyValidDestBlocks = filter (`Set.member` validDests) destBlocks
  -- dests <- getDestinations gs srcBlock
  -- destsInFunc <- filterM (\destBlock -> getFunction gs destBlock >>= (srcFunc <==>)) dests
  return $ (srcBlock,) <$> onlyValidDestBlocks
   
getBasicBlockGraph :: GhidraState -> J.Function -> Ghidra (BasicBlockGraph BasicBlock)
getBasicBlockGraph gs func = do
  blocks <- getCodeBlocks gs func >>= traverse mkBasicBlock
  edges <- concatMapM (getEdgesForBlock gs $ Set.fromList blocks) blocks
  return $ BasicBlockGraph blocks edges

getMaxAddress :: BasicBlock -> Ghidra Address
getMaxAddress bb = runIO (Java.call (coerce (bb ^. #handle) :: J.AddressSetView) "getMaxAddress")
                   >>= Addr.mkAddress
