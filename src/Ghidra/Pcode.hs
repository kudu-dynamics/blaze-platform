{-# LANGUAGE DataKinds #-}
module Ghidra.Pcode where

import Ghidra.Prelude hiding (toList)


import Ghidra.State (GhidraState)
import qualified Language.Java as Java
import Ghidra.Instruction (getInstructions)
import Ghidra.Util (iteratorToList)
import qualified Ghidra.Types as J
import Ghidra.Util (maybeNull)
import Ghidra.Types.Pcode ( BareHighPcodeInstruction
                          , BareRawPcodeInstruction
                          , PcodeInstruction(PcodeInstruction)
                          , BarePcodeOp
                          , RawPcodeInstruction
                          , HighPcodeInstruction
                          )
import qualified Ghidra.Variable as Var
import Ghidra.Variable (HighVariable)


getPcode :: J.Instruction -> IO [J.PcodeOp]
getPcode x = Java.call x "getPcode" >>= Java.reify

getRawPcodeOps :: J.Addressable a => GhidraState -> a -> IO [J.PcodeOp]
getRawPcodeOps gs x = do
  instrs <- getInstructions gs x
  concatMapM getPcode instrs

getPcodeOpAST :: J.HighFunction -> J.Instruction -> IO [J.PcodeOpAST]
getPcodeOpAST hfunc instr = do
  addr <- J.toAddr instr
  iter :: J.Iterator J.PcodeOpAST <- Java.call hfunc "getPcodeOps" addr
  iteratorToList iter

getHighPcodeOps :: J.Addressable a => GhidraState -> J.HighFunction -> a -> IO [J.PcodeOpAST]
getHighPcodeOps gs hfunc x = getInstructions gs x >>= concatMapM (getPcodeOpAST hfunc)

getBarePcodeOp :: Coercible a J.PcodeOp => a -> IO BarePcodeOp
getBarePcodeOp x = do
  s :: Text <- Java.call x' "getMnemonic" >>= Java.reify
  case readMaybe s :: Maybe BarePcodeOp of
    Nothing -> error $ "Can't convert pcode op: " <> show s
    Just op -> return op
  where
    x' :: J.PcodeOp
    x' = coerce x
      
mkBareRawPcodeInstruction :: J.PcodeOp -> IO BareRawPcodeInstruction
mkBareRawPcodeInstruction x = PcodeInstruction
  <$> getBarePcodeOp x
  <*> (maybeNull <$> Java.call x "getOutput")
  <*> (Java.call x "getInputs" >>= Java.reify)

mkBareHighPcodeInstruction :: J.PcodeOpAST -> IO BareHighPcodeInstruction
mkBareHighPcodeInstruction x = PcodeInstruction
  <$> getBarePcodeOp x
  <*> (maybeNull <$> Java.call x "getOutput")
  <*> (Java.call x "getInputs" >>= Java.reify)

mkHighPcodeInstruction :: BareHighPcodeInstruction -> IO HighPcodeInstruction
mkHighPcodeInstruction = traverse f
  where
    f :: J.VarNodeAST -> IO HighVariable
    f v = Java.call v "getHigh" >>= Var.mkHighVariable

mkRawPcodeInstruction :: BareRawPcodeInstruction -> IO RawPcodeInstruction
mkRawPcodeInstruction = traverse Var.mkVarNode
