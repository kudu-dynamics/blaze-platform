{-# LANGUAGE DataKinds #-}
module Ghidra.Instruction
  ( module Ghidra.Instruction
  , J.Instruction
  ) where

import Ghidra.Prelude hiding (toList)

import qualified Language.Java as Java
import qualified Ghidra.Types as J
import Ghidra.Util (maybeNull)
import Ghidra.Types (toAddrSet)
import Ghidra.Types.Internal (Ghidra, runIO)
import qualified Foreign.JNI as JNI


fromAddr :: J.ProgramDB -> J.Address -> Ghidra (Maybe J.Instruction)
fromAddr prg addr = do
  listing :: J.Listing <- runIO $ Java.call prg "getListing"
  maybeNull <$> runIO (Java.call listing "getInstructionContaining" addr >>= JNI.newGlobalRef)
  
getInstructions :: J.Addressable a => J.ProgramDB -> a -> Ghidra [J.Instruction]
getInstructions prg x = do
  listing :: J.Listing <- runIO $ Java.call prg "getListing" >>= JNI.newGlobalRef
  addrSet :: J.AddressSetView <- coerce <$> toAddrSet x
  instrsIterator :: J.InstructionIterator <- runIO $ Java.call listing "getInstructions" addrSet True >>= JNI.newGlobalRef
  instructionIteratorToList instrsIterator
  
instructionIteratorToList :: J.InstructionIterator -> Ghidra [J.Instruction]
instructionIteratorToList x = do
  hasNext :: Bool <- runIO $ Java.call x "hasNext"
  if hasNext
    then do
      ref <- runIO $ Java.call x "next" >>= JNI.newGlobalRef
      (ref:) <$> instructionIteratorToList x
    else return []

getAddress :: J.Instruction -> Ghidra J.Address
getAddress x = runIO $ Java.call (coerce x :: J.InstructionDB) "getAddress"
