{-# LANGUAGE DataKinds #-}
module Ghidra.Pcode where

import Ghidra.Prelude hiding (toList, getConst)


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
import Ghidra.Variable (VarNode, HighVarNode)
import qualified Ghidra.Types.Pcode.Lifted as L
import Ghidra.Types.Pcode.Lifted (PcodeOp, Input(Input), Output(Output), Destination, AddressSpaceMap)
import Ghidra.Types.Address (AddressSpace)
import qualified Data.HashMap.Strict as HashMap

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

class CanBeDestination a where
  toDestination :: a -> Destination

instance CanBeDestination Var.VarType where
  toDestination (Var.Const n) = L.Relative n
  toDestination (Var.Addr addr) = L.Absolute addr

instance CanBeDestination VarNode where
  toDestination = toDestination . view #varType

instance CanBeDestination HighVarNode where
  toDestination = toDestination . view #varType

class GetConst a where
  getConst :: a -> Maybe Int64

instance GetConst Var.VarType where
  getConst (Var.Const n) = Just n
  getConst (Var.Addr _) = Nothing

instance GetConst VarNode where
  getConst = getConst . view #varType

instance GetConst HighVarNode where
  getConst = getConst . view #varType


liftPcodeInstruction
  :: forall a. (CanBeDestination a, GetConst a)
  => AddressSpaceMap
  -> PcodeInstruction a
  -> Maybe (PcodeOp a)
liftPcodeInstruction addressSpaceMap x = case x ^. #op of
  BOOL_AND -> binOp L.BOOL_AND
  BOOL_NEGATE -> unOp L.BOOL_NEGATE
  BOOL_OR -> binOp L.BOOL_OR
  BOOL_XOR -> binOp L.BOOL_XOR
  BRANCH -> L.BRANCH <$> inputDest 0
  BRANCHIND -> L.BRANCHIND <$> input 0
  CALL -> L.CALL <$> inputDest 0 <*> inputList 1
  CALLIND -> L.CALLIND <$> input 0 <*> inputList 1
  CALLOTHER -> unknown "CALLOTHER" -- L.CALLOTHER <$> input 0 <*> inputList 1
  CAST -> unOp L.CAST
  CBRANCH -> L.CBRANCH <$> inputDest 0 <*> input 1
  COPY -> binOp L.COPY
  CPOOLREF -> L.CPOOLREF <$> out <*> input 0 <*> input 1 <*> inputList 2
  EXTRACT -> unknown "EXTRACT" -- binOp L.EXTRACT
  FLOAT_ABS -> unOp L.FLOAT_ABS
  FLOAT_ADD -> binOp L.FLOAT_ADD
  FLOAT_CEIL -> unOp L.FLOAT_CEIL
  FLOAT_DIV -> binOp L.FLOAT_DIV
  FLOAT_EQUAL -> binOp L.FLOAT_EQUAL
  FLOAT_FLOAT2FLOAT -> unOp L.FLOAT_FLOAT2FLOAT
  FLOAT_FLOOR -> unOp L.FLOAT_FLOOR
  FLOAT_INT2FLOAT -> unOp L.FLOAT_INT2FLOAT
  FLOAT_LESS -> binOp L.FLOAT_LESS
  FLOAT_LESSEQUAL -> binOp L.FLOAT_LESSEQUAL
  FLOAT_MULT -> binOp L.FLOAT_MULT
  FLOAT_NAN -> unOp L.FLOAT_NAN
  FLOAT_NEG -> unOp L.FLOAT_NEG
  FLOAT_NOTEQUAL -> binOp L.FLOAT_NOTEQUAL
  FLOAT_ROUND -> unOp L.FLOAT_ROUND
  FLOAT_SQRT -> unOp L.FLOAT_SQRT
  FLOAT_SUB -> binOp L.FLOAT_SUB
  FLOAT_TRUNC -> unknown "FLOAT_TRUNC" -- unOp L.FLOAT_TRUNC
  INDIRECT -> binOp L.INDIRECT
  INSERT -> unknown "INSERT"
  INT_2COMP -> unOp L.INT_2COMP
  INT_ADD -> binOp L.INT_ADD
  INT_AND -> binOp L.INT_AND
  INT_CARRY -> binOp L.INT_CARRY
  INT_DIV -> binOp L.INT_DIV
  INT_EQUAL -> binOp L.INT_EQUAL
  INT_LEFT -> binOp L.INT_LEFT
  INT_LESS -> binOp L.INT_LESS
  INT_LESSEQUAL -> binOp L.INT_LESSEQUAL
  INT_MULT -> binOp L.INT_MULT
  INT_NEGATE -> unOp L.INT_NEGATE
  INT_NOTEQUAL -> binOp L.INT_NOTEQUAL
  INT_OR -> binOp L.INT_OR
  INT_REM -> binOp L.INT_REM
  INT_RIGHT -> binOp L.INT_RIGHT
  INT_SBORROW -> binOp L.INT_SBORROW
  INT_SCARRY -> binOp L.INT_SCARRY
  INT_SDIV -> binOp L.INT_SDIV
  INT_SEXT -> unOp L.INT_SEXT
  INT_SLESS -> binOp L.INT_SLESS
  INT_SLESSEQUAL -> binOp L.INT_SLESSEQUAL
  INT_SREM -> binOp L.INT_SREM
  INT_SRIGHT -> binOp L.INT_SRIGHT
  INT_SUB -> binOp L.INT_SUB
  INT_XOR -> binOp L.INT_XOR
  INT_ZEXT -> unOp L.INT_ZEXT
  LOAD -> L.LOAD <$> out <*> inputAddressSpace 0 <*> input 1
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
  where
    out :: Maybe (Output a)
    out = Output <$> x ^. #output

    input :: Word64 -> Maybe (Input a)
    input n = Input n <$> x ^? #inputs . ix (fromIntegral n )

    inputList :: Word64 -> Maybe [Input a]
    inputList startingInput = traverse input
      . drop (fromIntegral startingInput)
      . fmap fst
      . zip [0..] 
      $ x ^. #inputs

    inputDest :: Word64 -> Maybe (Input Destination)
    inputDest n = fmap toDestination <$> input n

    inputAddressSpace :: Word64 -> Maybe (Input AddressSpace)
    inputAddressSpace n = input n >>= traverse f
      where
        f :: a -> Maybe AddressSpace
        f a = do
          spaceId <- fromIntegral <$> getConst a
          HashMap.lookup spaceId addressSpaceMap

    binOp :: (Output a -> Input a -> Input a -> PcodeOp a) -> Maybe (PcodeOp a)
    binOp constructor = constructor <$> out <*> input 0 <*> input 1

    unOp :: (Output a -> Input a -> PcodeOp a) -> Maybe (PcodeOp a)
    unOp constructor = constructor <$> out <*> input 0

    unknown :: forall b. Text -> b
    unknown t = error $ "Encountered undocumented pcode op: " <> cs t
