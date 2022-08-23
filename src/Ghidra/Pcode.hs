{-# LANGUAGE DataKinds #-}
module Ghidra.Pcode where

import Ghidra.Prelude hiding (toList)

import Language.Clojure
import System.IO.Memoize (once)
import Foreign.JNI.Types (JObject)
import qualified Data.BinaryAnalysis as BA
import qualified Ghidra.Program as Program
import Ghidra.State (GhidraState(GhidraState))
import qualified Ghidra.State as State
import qualified Language.Java as Java
import qualified Ghidra.Instruction as Instruction
import Ghidra.Instruction (getInstructions, Instruction)
import Ghidra.Function (HighFunction)
import Ghidra.Util (convertOpt , iteratorToList)
import Language.Java (J)
import Ghidra.Types


type PcodeOp = J ('Java.Class "ghidra.program.model.pcode.PcodeOp")

type PcodeOpAST = J ('Java.Class "ghidra.program.model.pcode.PcodeOpAST")

-- type PcodeOpASTIterator = J ('Java.Class "java.util.Iterator<ghidra.program.model.pcode.PcodeOpAST>")

-- iteratorToList :: PcodeOpASTIterator -> IO [PcodeOpAST]
-- iteratorToList it = do
--   Java.call it "hasNext" >>= \case
--     False -> return []
--     True -> do
--       x <- Java.call it "next"
--       (x:) <$> iteratorToList it


getPcode :: Instruction -> IO [PcodeOp]
getPcode x = Java.call x "getPcode" >>= Java.reify

getRawPcodeOps :: Addressable a => GhidraState -> a -> IO [PcodeOp]
getRawPcodeOps gs x = do
  instrs <- getInstructions gs x
  concatMapM getPcode instrs

getPcodeOpAST :: HighFunction -> Instruction -> IO [PcodeOpAST]
getPcodeOpAST hfunc instr = do
  addr <- toAddr instr
  iter :: Iterator PcodeOpAST <- Java.call hfunc "getPcodeOps" addr
  -- iter :: PcodeOpASTIterator <- Java.call hfunc "getPcodeOps" addr
  iteratorToList iter

getHighPcodeOps :: Addressable a => GhidraState -> HighFunction -> a -> IO [PcodeOpAST]
getHighPcodeOps gs hfunc x = getInstructions gs x >>= concatMapM (getPcodeOpAST hfunc)
