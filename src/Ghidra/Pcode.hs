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
                          , BarePcodeOp(..)
                          )
import qualified Ghidra.Variable as Var
import qualified Ghidra.Types.Pcode.Lifted as L
import Ghidra.Types.Pcode.Lifted (PcodeOp)


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
mkHighPcodeInstruction = traverse Var.mkHighVarNode

mkRawPcodeInstruction :: BareRawPcodeInstruction -> IO RawPcodeInstruction
mkRawPcodeInstruction = traverse Var.mkVarNode

-- runPcodeParser :: Parser a -> ParserCtx -> IO (P

liftPcodeInstruction :: PcodeInstruction a -> Maybe (PcodeOp a)
liftPcodeInstruction x = case x ^. #op of
  BOOL_AND -> undefined -- L.BOOL_AND <$> out x <*> 
  BOOL_NEGATE -> undefined
  BOOL_OR -> undefined
  BOOL_XOR -> undefined
  BRANCH -> undefined
  BRANCHIND -> undefined
  CALL -> undefined
  CALLIND -> undefined
  CALLOTHER -> undefined
  CAST -> undefined
  CBRANCH -> undefined
  COPY -> undefined
  CPOOLREF -> undefined
  EXTRACT -> undefined
  FLOAT_ABS -> undefined
  FLOAT_ADD -> undefined
  FLOAT_CEIL -> undefined
  FLOAT_DIV -> undefined
  FLOAT_EQUAL -> undefined
  FLOAT_FLOAT2FLOAT -> undefined
  FLOAT_FLOOR -> undefined
  FLOAT_INT2FLOAT -> undefined
  FLOAT_LESS -> undefined
  FLOAT_LESSEQUAL -> undefined
  FLOAT_MULT -> undefined
  FLOAT_NAN -> undefined
  FLOAT_NEG -> undefined
  FLOAT_NOTEQUAL -> undefined
  FLOAT_ROUND -> undefined
  FLOAT_SQRT -> undefined
  FLOAT_SUB -> undefined
  FLOAT_TRUNC -> undefined
  INDIRECT -> undefined
  INSERT -> undefined
  INT_2COMP -> undefined
  INT_ADD -> undefined
  INT_AND -> undefined
  INT_CARRY -> undefined
  INT_DIV -> undefined
  INT_EQUAL -> undefined
  INT_LEFT -> undefined
  INT_LESS -> undefined
  INT_LESSEQUAL -> undefined
  INT_MULT -> undefined
  INT_NEGATE -> undefined
  INT_NOTEQUAL -> undefined
  INT_OR -> undefined
  INT_REM -> undefined
  INT_RIGHT -> undefined
  INT_SBORROW -> undefined
  INT_SCARRY -> undefined
  INT_SDIV -> undefined
  INT_SEXT -> undefined
  INT_SLESS -> undefined
  INT_SLESSEQUAL -> undefined
  INT_SREM -> undefined
  INT_SRIGHT -> undefined
  INT_SUB -> undefined
  INT_XOR -> undefined
  INT_ZEXT -> undefined
  LOAD -> undefined
  MULTIEQUAL -> undefined
  NEW -> undefined
  PCODE_MAX -> undefined
  PIECE -> undefined
  POPCOUNT -> undefined
  PTRADD -> undefined
  PTRSUB -> undefined
  RETURN -> undefined
  SEGMENTOP -> undefined
  STORE -> undefined
  SUBPIECE -> undefined
  UNIMPLEMENTED -> undefined
