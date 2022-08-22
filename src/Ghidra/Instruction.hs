{-# LANGUAGE DataKinds #-}
module Ghidra.Instruction
  ( module Ghidra.Instruction
  ) where

import Ghidra.Prelude hiding (toList)

import Language.Clojure
import System.IO.Memoize (once)
import Foreign.JNI.Types (JObject)
import qualified Data.BinaryAnalysis as BA
import qualified Ghidra.Program as Program
import Ghidra.State (GhidraState(GhidraState))
import qualified Ghidra.State as State
import qualified Language.Java as Java
import Ghidra.Util (convertOpt)
import Language.Java (J)
import Ghidra.Types


requireModule :: IO ()
requireModule = unsafePerformIO . once $ do
  _ <- readEval "(require (quote [ghidra-clojure.function]))"
  return ()

type Instruction = J ('Java.Class "ghidra.program.model.listing.Instruction")
type InstructionIterator = J ('Java.Class "ghidra.program.model.listing.InstructionIterator")

instance Addressable Instruction where
  toAddr x = Java.call x "getAddress"
  toAddrSet = Java.new <=< toAddr

fromAddr :: GhidraState -> Address -> IO (Maybe Instruction)
fromAddr gs addr = do
  prg <- State.getProgram gs
  listing :: Listing <- Java.call prg "getListing"
  maybeNull <$> Java.call listing "getInstructionContaining" addr
  
getInstructions :: Addressable a => GhidraState -> a -> IO [Instruction]
getInstructions gs x = do
  prg <- State.getProgram gs
  listing :: Listing <- Java.call prg "getListing"
  addrSet <- toAddrSet x
  instrsIterator :: InstructionIterator <- Java.call listing "getInstructions" addrSet True
  instructionIteratorToList instrsIterator
  
instructionIteratorToList :: InstructionIterator -> IO [Instruction]
instructionIteratorToList x = do
  hasNext :: Bool <- Java.call x "hasNext"
  if hasNext
    then do
      ref <- Java.call x "next"
      (ref:) <$> instructionIteratorToList x
    else return []
