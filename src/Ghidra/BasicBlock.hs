{-# LANGUAGE DataKinds #-}
module Ghidra.BasicBlock
  ( module Ghidra.BasicBlock
  ) where

import Ghidra.Prelude hiding (toList)

import Ghidra.State (GhidraState)
import qualified Ghidra.State as State
import Ghidra.Types.BasicBlock (BasicBlockGraph(BasicBlockGraph))
import qualified Language.Java as Java
import qualified Ghidra.Address as Addr
import qualified Foreign.JNI as JNI
import qualified Ghidra.Types as J
import Ghidra.Types.Address (Address)
import Ghidra.Types.BasicBlock (BasicBlock(BasicBlock))
import qualified Data.Set as Set


codeBlockIteratorToList :: J.CodeBlockIterator -> IO [J.CodeBlock]
codeBlockIteratorToList x = do
  hasNext :: Bool <- Java.call x "hasNext"
  if hasNext
    then do
      ref <- Java.call x "next" >>= JNI.newGlobalRef
      (ref:) <$> codeBlockIteratorToList x
    else return []

codeBlockReferenceIteratorToList :: J.CodeBlockReferenceIterator -> IO [J.CodeBlockReference]
codeBlockReferenceIteratorToList x = do
  hasNext :: Bool <- Java.call x "hasNext"
  if hasNext
    then do
      ref <- Java.call x "next" >>= JNI.newGlobalRef
      (ref:) <$> codeBlockReferenceIteratorToList x
    else return []

getCodeBlocks :: J.Addressable a => GhidraState -> a -> IO [J.CodeBlock]
getCodeBlocks gs x = do
  prg <- State.getProgram gs
  bbModel :: J.BasicBlockModel <- Java.new (coerce prg :: J.Program)
  
  tm <- State.getTaskMonitor gs
  addrSet :: J.AddressSetView <- coerce $ J.toAddrSet x
  Java.call bbModel "getCodeBlocksContaining" addrSet tm >>= codeBlockIteratorToList

getDestinations :: GhidraState -> J.CodeBlock -> IO [J.CodeBlock]
getDestinations gs block = do
  tm <- State.getTaskMonitor gs
  blocks <- Java.call block "getDestinations" tm >>= codeBlockReferenceIteratorToList
  mapM (JNI.newGlobalRef <=< (\ref -> Java.call ref "getDestinationBlock")) blocks

getSources :: GhidraState -> J.CodeBlock -> IO [J.CodeBlock]
getSources gs block = do
  tm <- State.getTaskMonitor gs
  blocks <- Java.call block "getSources" tm >>= codeBlockReferenceIteratorToList
  mapM (JNI.newGlobalRef <=< (\ref -> Java.call ref "getSourceBlock")) blocks

getStartAddress :: J.CodeBlock -> IO Address
getStartAddress block
  = Java.call block "getFirstStartAddress" >>= Addr.mkAddress

getFunction :: GhidraState -> J.CodeBlock -> IO J.Function
getFunction gs block = do
  prg <- State.getProgram gs
  listing :: J.Listing <- Java.call prg "getListing"
  blockAddr <- J.toAddr block
  Java.call listing "getFunctionContaining" blockAddr

shareFunc :: GhidraState -> J.CodeBlock -> J.CodeBlock -> IO Bool
shareFunc gs a b = do
  funcA <- getFunction gs a
  funcB <- getFunction gs b
  return $ funcA == funcB

mkBasicBlock :: J.CodeBlock -> IO BasicBlock
mkBasicBlock block = BasicBlock block <$> getStartAddress block
  
getEdgesForBlock
  :: GhidraState
  -> Set BasicBlock
  -> BasicBlock
  -> IO [(BasicBlock, BasicBlock)]
getEdgesForBlock gs validDests srcBlock  = do
  destBlocks :: [BasicBlock] <- getDestinations gs (srcBlock ^. #handle) >>= traverse mkBasicBlock
  let onlyValidDestBlocks = filter (`Set.member` validDests) destBlocks
  -- dests <- getDestinations gs srcBlock
  -- destsInFunc <- filterM (\destBlock -> getFunction gs destBlock >>= (srcFunc <==>)) dests
  return $ (srcBlock,) <$> onlyValidDestBlocks
   
getBasicBlockGraph :: GhidraState -> J.Function -> IO (BasicBlockGraph BasicBlock)
getBasicBlockGraph gs func = do
  blocks <- getCodeBlocks gs func >>= traverse mkBasicBlock
  edges <- concatMapM (getEdgesForBlock gs $ Set.fromList blocks) blocks
  return $ BasicBlockGraph blocks edges

getMaxAddress :: BasicBlock -> IO Address
getMaxAddress bb = Java.call (coerce (bb ^. #handle) :: J.AddressSetView) "getMaxAddress"
                   >>= Addr.mkAddress
