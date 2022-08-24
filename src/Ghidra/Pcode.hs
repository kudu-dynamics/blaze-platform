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
import Ghidra.Instruction (getInstructions)
import Ghidra.Util (convertOpt , iteratorToList)
import Language.Java (J)
import qualified Ghidra.Types as J
import Ghidra.Util (maybeNull)
import Ghidra.Types.Pcode (BarePcodeInstruction(BarePcodeInstruction), BarePcodeOp)

type AddressSpace = Int
type Offset = Int

data AddressOffset = AddressOffset
  { addressSpace :: AddressSpace
  , offset :: Offset
  } deriving (Eq, Ord, Show, Generic, Hashable)

-- pcodeOps :: [(Text, [(Text, Text)])]
--   [ ( "BOOL_AND"
--     , [ ("input0", "var")
--       , ("input1", "var")
--       , ("output", "var")
--       ])
--   , ( "BOOL_NEGATE"
--     , [ ("input0", "var")
--       , ("output", "var")
--       ])
--   , ( "BOOL_OR"
--     , [ ("input0", "var")
--       , ("input1", "var")
--       , ("output", "var")
--       ])
--   , ( "BOOL_XOR"
--     , [ ("input0", "var")
--       , ("input1", "var")
--       , ("output", "var")
--       ])
--   , ( "BRANCH"
--     , [ ("input0", "AddressOffset")
--       ])
--   , ( "BRANCHIND"
--     , [ ("input0", "var")
--       ])
--   , ( "CALL"
--     , [ ("input0", "AddressOffset")
--       , ("args", "[var]")
--       ])
--   , ( "CALLIND"
--     , [ ("input0", "var")
--       , ("args", "[var]")
--       ])
--   , ( "CALLOTHER"
--     , []) -- TODO
--   , ( "CAST"
--     , [ ("input0", "var")
--       , ("output", "var")
--       ])
--   , ( "CBRANCH"
--     , [ ("input0", "AddressOffset")
--       , ("input1", "var") -- condition
--       ])
--   , ( "COPY"
--     , [ ("input0", "var")
--       , ("output", "var")
--       ])
    
--   | CPOOLREF
--   | EXTRACT
--   | FLOAT_ABS
--   | FLOAT_ADD
--   | FLOAT_CEIL
--   | FLOAT_DIV
--   | FLOAT_EQUAL
--   | FLOAT_FLOAT2FLOAT
--   | FLOAT_FLOOR
--   | FLOAT_INT2FLOAT
--   | FLOAT_LESS
--   | FLOAT_LESSEQUAL
--   | FLOAT_MULT
--   | FLOAT_NAN
--   | FLOAT_NEG
--   | FLOAT_NOTEQUAL
--   | FLOAT_ROUND
--   | FLOAT_SQRT
--   | FLOAT_SUB
--   | FLOAT_TRUNC
--   | INDIRECT
--   | INSERT
--   | INT_2COMP
--   | INT_ADD
--   | INT_AND
--   | INT_CARRY
--   | INT_DIV
--   | INT_EQUAL
--   | INT_LEFT
--   | INT_LESS
--   | INT_LESSEQUAL
--   | INT_MULT
--   | INT_NEGATE
--   | INT_NOTEQUAL
--   | INT_OR
--   | INT_REM
--   | INT_RIGHT
--   | INT_SBORROW
--   | INT_SCARRY
--   | INT_SDIV
--   | INT_SEXT
--   | INT_SLESS
--   | INT_SLESSEQUAL
--   | INT_SREM
--   | INT_SRIGHT
--   | INT_SUB
--   | INT_XOR
--   | INT_ZEXT
--   | LOAD
--   | MULTIEQUAL
--   | NEW
--   | PCODE_MAX
--   | PIECE
--   | POPCOUNT
--   | PTRADD
--   | PTRSUB
--   | RETURN
--   | SEGMENTOP
--   | STORE
--   | SUBPIECE
--   | UNIMPLEMENTED


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

toBareRawPcodeInstruction :: Coercible a J.PcodeOp => a -> IO BarePcodeInstruction
toBareRawPcodeInstruction x = do
  s :: Text <- Java.call x' "getMnemonic" >>= Java.reify
  case readMaybe s :: Maybe BarePcodeOp of
    Nothing -> error $ "Can't convert pcode op: " <> show s
    Just op -> do
      inputs :: [J.VarNode] <- Java.call x' "getInputs" >>= Java.reify
      moutput <- maybeNull <$> Java.call x' "getOutput"
      return $ BarePcodeInstruction op moutput inputs
  where
    x' :: J.PcodeOp
    x' = coerce x


