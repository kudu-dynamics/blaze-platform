module Binja.MLIL
  ( module Exports
  , getInstructionCount
  , expression
  , instruction
  , fromBasicBlock
  , fromFunction
  , getMediumLevelILInstructionByInstructionIndex
  , getMediumLevelILInstructionByExpressionIndex
  , getMLILFromLLIL
  , getMLILSSSAFromMLIL
  , getParams
  ) where

import           Binja.Prelude
import qualified Prelude as P

import           Data.Binary.IEEE754             ( wordToDouble
                                                 , wordToFloat
                                                 )
import           GHC.Float                       ( float2Double )
import           Binja.BasicBlock                ( BasicBlock )
import qualified Binja.BasicBlock     as BB
import qualified Binja.C.Enums        as BN
import qualified Binja.C.Main         as BN
import           Binja.C.Pointers
import           Binja.C.Types
import           Binja.Function                  ( Function
                                                 , MLILFunction
                                                 , MLILSSAFunction
                                                 , LLILFunction
                                                 )
import qualified Binja.Function       as Func
import           Binja.Types.MLIL     as Exports
import           Binja.Types.Variable            ( Variable )
import qualified Binja.Variable       as Var
import Binja.C.Util (isNil)

class ( Func.HasHandle fun BNMediumLevelILFunction
      , Func.HasFunc fun Function ) => StatementFunction fun where
  getExprIndex :: fun -> InstructionIndex fun -> IO (ExpressionIndex fun)

instance StatementFunction MLILFunction where
  getExprIndex fn iindex = do
    whenM (isNil fnPtr) (P.error "Function handle is nil for MLILFunction.")

    BN.getMediumLevelILIndexForInstruction fnPtr (coerceInstructionIndex iindex)
    where
      fnPtr = fn ^. Func.handle

instance StatementFunction MLILSSAFunction where
  getExprIndex fn iindex = do
    whenM (isNil fnPtr) (P.error "Function handle is nil MLILSSAFunction.")

    BN.getMediumLevelILIndexForInstruction fnPtr (coerceInstructionIndex iindex)
    >>= BN.getMediumLevelILSSAExprIndex fnPtr
    where
      fnPtr = fn ^. Func.handle

getMLILFromLLIL :: LLILFunction -> InstructionIndex LLILFunction -> IO (InstructionIndex MLILFunction)
getMLILFromLLIL fn = BN.getMediumLevelILInstructionIndexFromLLIL (fn ^. Func.handle)

getMLILSSSAFromMLIL :: MLILFunction -> InstructionIndex MLILFunction -> IO (InstructionIndex MLILSSAFunction)
getMLILSSSAFromMLIL fn = BN.getMediumLevelILSSAInstructionIndexFromMLIL (fn ^. Func.handle)

getInstructionCount :: StatementFunction fun => fun -> IO Word64
getInstructionCount = BN.getMediumLevelILInstructionCount . view Func.handle

getMediumLevelILInstructionByExpressionIndex :: StatementFunction fun
                         => fun -> ExpressionIndex fun
                         -> IO MediumLevelILInstruction
getMediumLevelILInstructionByExpressionIndex fn eindex =
  BN.getMediumLevelILByIndex (fn ^. Func.handle) (coerceExpressionIndex eindex)

getMediumLevelILInstructionByInstructionIndex :: StatementFunction fun
                          => fun -> InstructionIndex fun
                          -> IO MediumLevelILInstruction
getMediumLevelILInstructionByInstructionIndex fn iindex = getExprIndex fn iindex >>= getMediumLevelILInstructionByExpressionIndex fn

buildInt :: OpBuilder t Int64
buildInt = fromIntegral <$> takeOpDataWord

buildFloat :: OpBuilder t Double
buildFloat = do
  sz <- view size <$> ask
  w <- takeOpDataWord
  case sz of
    4 -> return . float2Double . wordToFloat . fromIntegral $ w
    8 -> return . wordToDouble $ w
    _ -> putText "buildFloat: Invalid size. Defaulting to 0.0" >> return 0.0

buildVariable :: StatementFunction t => OpBuilder t Variable
buildVariable = do
  vid <- fromIntegral <$> takeOpDataWord
  fn <- view (func . Func.func) <$> ask
  liftIO $ Var.getVariableFromIdentifier fn vid

buildSSAVariable :: StatementFunction t => OpBuilder t SSAVariable
buildSSAVariable = do
  v <- buildVariable
  vversion <- fromIntegral <$> takeOpDataWord
  return $ SSAVariable v vversion

buildSSAVariableDestAndSrc :: StatementFunction t => OpBuilder t SSAVariableDestAndSrc
buildSSAVariableDestAndSrc = do
  v <- buildVariable
  destVersion <- fromIntegral <$> takeOpDataWord
  srcVersion <- fromIntegral <$> takeOpDataWord
  return $ SSAVariableDestAndSrc { _dest = SSAVariable v destVersion
                                 , _src = SSAVariable v srcVersion }

buildIntList :: StatementFunction t => OpBuilder t [Int64]
buildIntList = do
  ctx <- ask
  oindex <- getAndAdvanceOpIndex
  liftIO $ BN.mediumLevelILGetOperandList
    (ctx ^. func . Func.handle)
    (coerceExpressionIndex $ ctx ^. exprIndex)
    oindex

buildVarList :: StatementFunction t => OpBuilder t [Variable]
buildVarList = do
  xs <- buildIntList
  fn <- view (func . Func.func) <$> ask
  void getAndAdvanceOpIndex  -- I don't know why...
  liftIO $ traverse (Var.getVariableFromIdentifier fn . fromIntegral) xs

asPairs :: [a] -> [(a, a)]
asPairs [] = []
asPairs [_] = []
asPairs (x:y:xs) = (x, y) : asPairs xs

buildSSAVarList :: StatementFunction t => OpBuilder t [SSAVariable]
buildSSAVarList = do
  xs <- buildIntList
  fn <- view (func . Func.func) <$> ask
  void getAndAdvanceOpIndex  -- I don't know why...
  liftIO . traverse (f fn) $ asPairs xs
  where
    f fn (vid, vversion) = do
      v <- Var.getVariableFromIdentifier fn (fromIntegral vid)
      return $ SSAVariable v (fromIntegral vversion)

buildExprList :: StatementFunction t => OpBuilder t [Expression t]
buildExprList = do
  xs <- buildIntList
  fn <- view func <$> ask
  _ <- getAndAdvanceOpIndex  -- I don't know why...
  liftIO $ traverse (expression fn . fromIntegral) xs

buildIntrinsinc :: OpBuilder t Intrinsic
buildIntrinsinc = fromIntegral <$> takeOpDataWord

buildExpr :: StatementFunction t => OpBuilder t (Expression t)
buildExpr = do
  w <- takeOpDataWord
  fn <- view func <$> ask
  liftIO $ expression fn (fromIntegral w)

expression :: StatementFunction t => t -> ExpressionIndex t -> IO (Expression t)
expression fn eindex = do
  (mlil, op') <- getOperation fn eindex
  return $ Expression { _address = mlil ^. address
                      , _index = eindex
                      , _size = mlil ^. size
                      , _op = op' }

instruction :: StatementFunction t => t -> InstructionIndex t -> IO (Instruction t)
instruction fn iindex = do
  eindex <- getExprIndex fn iindex
  (mlil, op') <- getOperation fn eindex
  return $ Instruction { _address = mlil ^. address
                       , _index = iindex
                       , _size = mlil ^. size
                       , _op = op' }

fromBasicBlock :: StatementFunction t => BasicBlock t -> IO [Instruction t]
fromBasicBlock bb = mapM (instruction $ bb ^. BB.func) [(bb ^. BB.start) .. (bb ^. BB.end) - 1]

fromFunction :: StatementFunction fun => fun -> IO [Instruction fun]
fromFunction fn = do
  len <- getInstructionCount fn
  mapM (instruction fn) [0 .. fromIntegral len - 1]

getOperation :: StatementFunction t
            => t -> ExpressionIndex t -> IO (MediumLevelILInstruction, Operation (Expression t))
getOperation fn eindex = do
  mlil <- getMediumLevelILInstructionByExpressionIndex fn eindex
  let ctx = OpBuilderCtx { _func = fn
                         , _exprIndex = eindex
                         , _opData = mlil ^. operands
                         , _size = mlil ^. size }
      st = 0
  fmap (mlil,) . flip runOpBuilder (ctx, st) $ case mlil ^. operation of
    BN.MLIL_NOP -> return NOP
    BN.MLIL_SET_VAR ->
        fmap SET_VAR $ SetVarOp <$> buildVariable <*> buildExpr
    BN.MLIL_SET_VAR_FIELD ->
        fmap SET_VAR_FIELD $ SetVarFieldOp <$> buildVariable <*> buildInt <*> buildExpr
    BN.MLIL_SET_VAR_SPLIT ->
        fmap SET_VAR_SPLIT $ SetVarSplitOp <$> buildVariable <*> buildVariable <*> buildExpr
    BN.MLIL_LOAD ->
        fmap LOAD $ LoadOp <$> buildExpr
    BN.MLIL_LOAD_STRUCT ->
        fmap LOAD_STRUCT $ LoadStructOp <$> buildExpr <*> buildInt
    BN.MLIL_STORE ->
        fmap STORE $ StoreOp <$> buildExpr <*> buildExpr
    BN.MLIL_STORE_STRUCT ->
        fmap STORE_STRUCT $ StoreStructOp <$> buildExpr <*> buildInt <*> buildExpr
    BN.MLIL_VAR ->
        fmap VAR $ VarOp <$> buildVariable
    BN.MLIL_VAR_FIELD ->
        fmap VAR_FIELD $ VarFieldOp <$> buildVariable <*> buildInt
    BN.MLIL_VAR_SPLIT ->
        fmap VAR_SPLIT $ VarSplitOp <$> buildVariable <*> buildVariable
    BN.MLIL_ADDRESS_OF ->
        fmap ADDRESS_OF $ AddressOfOp <$> buildVariable
    BN.MLIL_ADDRESS_OF_FIELD ->
        fmap ADDRESS_OF_FIELD $ AddressOfFieldOp <$> buildVariable <*> buildInt
    BN.MLIL_CONST ->
        fmap CONST $ ConstOp <$> buildInt
    BN.MLIL_CONST_PTR ->
        fmap CONST_PTR $ ConstPtrOp <$> buildInt
    BN.MLIL_EXTERN_PTR ->
        fmap EXTERN_PTR $ ExternPtrOp <$> buildInt <*> buildInt
    BN.MLIL_FLOAT_CONST ->
        fmap FLOAT_CONST $ FloatConstOp <$> buildFloat
    BN.MLIL_IMPORT ->
        fmap IMPORT $ ImportOp <$> buildInt
    BN.MLIL_ADD ->
        fmap ADD $ AddOp <$> buildExpr <*> buildExpr
    BN.MLIL_ADC ->
        fmap ADC $ AdcOp <$> buildExpr <*> buildExpr <*> buildExpr
    BN.MLIL_SUB ->
        fmap SUB $ SubOp <$> buildExpr <*> buildExpr
    BN.MLIL_SBB ->
        fmap SBB $ SbbOp <$> buildExpr <*> buildExpr <*> buildExpr
    BN.MLIL_AND ->
        fmap AND $ AndOp <$> buildExpr <*> buildExpr
    BN.MLIL_OR ->
        fmap OR $ OrOp <$> buildExpr <*> buildExpr
    BN.MLIL_XOR ->
        fmap XOR $ XorOp <$> buildExpr <*> buildExpr
    BN.MLIL_LSL ->
        fmap LSL $ LslOp <$> buildExpr <*> buildExpr
    BN.MLIL_LSR ->
        fmap LSR $ LsrOp <$> buildExpr <*> buildExpr
    BN.MLIL_ASR ->
        fmap ASR $ AsrOp <$> buildExpr <*> buildExpr
    BN.MLIL_ROL ->
        fmap ROL $ RolOp <$> buildExpr <*> buildExpr
    BN.MLIL_RLC ->
        fmap RLC $ RlcOp <$> buildExpr <*> buildExpr <*> buildExpr
    BN.MLIL_ROR ->
        fmap ROR $ RorOp <$> buildExpr <*> buildExpr
    BN.MLIL_RRC ->
        fmap RRC $ RrcOp <$> buildExpr <*> buildExpr <*> buildExpr
    BN.MLIL_MUL ->
        fmap MUL $ MulOp <$> buildExpr <*> buildExpr
    BN.MLIL_MULU_DP ->
        fmap MULU_DP $ MuluDpOp <$> buildExpr <*> buildExpr
    BN.MLIL_MULS_DP ->
        fmap MULS_DP $ MulsDpOp <$> buildExpr <*> buildExpr
    BN.MLIL_DIVU ->
        fmap DIVU $ DivuOp <$> buildExpr <*> buildExpr
    BN.MLIL_DIVU_DP ->
        fmap DIVU_DP $ DivuDpOp <$> buildExpr <*> buildExpr
    BN.MLIL_DIVS ->
        fmap DIVS $ DivsOp <$> buildExpr <*> buildExpr
    BN.MLIL_DIVS_DP ->
        fmap DIVS_DP $ DivsDpOp <$> buildExpr <*> buildExpr
    BN.MLIL_MODU ->
        fmap MODU $ ModuOp <$> buildExpr <*> buildExpr
    BN.MLIL_MODU_DP ->
        fmap MODU_DP $ ModuDpOp <$> buildExpr <*> buildExpr
    BN.MLIL_MODS ->
        fmap MODS $ ModsOp <$> buildExpr <*> buildExpr
    BN.MLIL_MODS_DP ->
        fmap MODS_DP $ ModsDpOp <$> buildExpr <*> buildExpr
    BN.MLIL_NEG ->
        fmap NEG $ NegOp <$> buildExpr
    BN.MLIL_NOT ->
        fmap NOT $ NotOp <$> buildExpr
    BN.MLIL_SX ->
        fmap SX $ SxOp <$> buildExpr
    BN.MLIL_ZX ->
        fmap ZX $ ZxOp <$> buildExpr
    BN.MLIL_LOW_PART ->
        fmap LOW_PART $ LowPartOp <$> buildExpr
    BN.MLIL_JUMP ->
        fmap JUMP $ JumpOp <$> buildExpr
    BN.MLIL_JUMP_TO ->
        fmap JUMP_TO $ JumpToOp <$> buildExpr <*> buildIntList
    BN.MLIL_RET_HINT ->
        fmap RET_HINT $ RetHintOp <$> buildExpr
    BN.MLIL_CALL ->
        fmap CALL $ CallOp <$> buildVarList <*> buildExpr <*> buildExprList
    BN.MLIL_CALL_UNTYPED ->
        fmap CALL_UNTYPED $ CallUntypedOp <$> buildExpr <*> buildExpr <*> buildExpr <*> buildExpr
    BN.MLIL_CALL_OUTPUT ->
        fmap CALL_OUTPUT $ CallOutputOp <$> buildVarList
    BN.MLIL_CALL_PARAM ->
        fmap CALL_PARAM $ CallParamOp <$> buildVarList
    BN.MLIL_RET ->
        fmap RET $ RetOp <$> buildExprList
    BN.MLIL_NORET -> return NORET
    BN.MLIL_IF ->
        fmap IF $ IfOp <$> buildExpr <*> buildInt <*> buildInt
    BN.MLIL_GOTO ->
        fmap GOTO $ GotoOp <$> buildInt
    BN.MLIL_CMP_E ->
        fmap CMP_E $ CmpEOp <$> buildExpr <*> buildExpr
    BN.MLIL_CMP_NE ->
        fmap CMP_NE $ CmpNeOp <$> buildExpr <*> buildExpr
    BN.MLIL_CMP_SLT ->
        fmap CMP_SLT $ CmpSltOp <$> buildExpr <*> buildExpr
    BN.MLIL_CMP_ULT ->
        fmap CMP_ULT $ CmpUltOp <$> buildExpr <*> buildExpr
    BN.MLIL_CMP_SLE ->
        fmap CMP_SLE $ CmpSleOp <$> buildExpr <*> buildExpr
    BN.MLIL_CMP_ULE ->
        fmap CMP_ULE $ CmpUleOp <$> buildExpr <*> buildExpr
    BN.MLIL_CMP_SGE ->
        fmap CMP_SGE $ CmpSgeOp <$> buildExpr <*> buildExpr
    BN.MLIL_CMP_UGE ->
        fmap CMP_UGE $ CmpUgeOp <$> buildExpr <*> buildExpr
    BN.MLIL_CMP_SGT ->
        fmap CMP_SGT $ CmpSgtOp <$> buildExpr <*> buildExpr
    BN.MLIL_CMP_UGT ->
        fmap CMP_UGT $ CmpUgtOp <$> buildExpr <*> buildExpr
    BN.MLIL_TEST_BIT ->
        fmap TEST_BIT $ TestBitOp <$> buildExpr <*> buildExpr
    BN.MLIL_BOOL_TO_INT ->
        fmap BOOL_TO_INT $ BoolToIntOp <$> buildExpr
    BN.MLIL_ADD_OVERFLOW ->
        fmap ADD_OVERFLOW $ AddOverflowOp <$> buildExpr <*> buildExpr
    BN.MLIL_SYSCALL ->
        fmap SYSCALL $ SyscallOp <$> buildVarList <*> buildExprList
    BN.MLIL_SYSCALL_UNTYPED ->
        fmap SYSCALL_UNTYPED $ SyscallUntypedOp <$> buildExpr <*> buildExpr <*> buildExpr
    BN.MLIL_TAILCALL ->
        fmap TAILCALL $ TailcallOp <$> buildVarList <*> buildExpr <*> buildExprList
    BN.MLIL_TAILCALL_UNTYPED ->
        fmap TAILCALL_UNTYPED $ TailcallUntypedOp <$> buildExpr <*> buildExpr <*> buildExpr <*> buildExpr
    BN.MLIL_BP -> return BP
    BN.MLIL_TRAP ->
        fmap TRAP $ TrapOp <$> buildInt
    BN.MLIL_INTRINSIC ->
        fmap INTRINSIC $ IntrinsicOp <$> buildVarList <*> buildIntrinsinc <*> buildExprList
    BN.MLIL_INTRINSIC_SSA ->
        fmap INTRINSIC_SSA $ IntrinsicSSAOp <$> buildSSAVarList <*> buildIntrinsinc <*> buildExprList
    BN.MLIL_FREE_VAR_SLOT ->
        fmap FREE_VAR_SLOT $ FreeVarSlotOp <$> buildVariable
    BN.MLIL_FREE_VAR_SLOT_SSA ->
        fmap FREE_VAR_SLOT_SSA $ FreeVarSlotSSAOp <$> buildSSAVariableDestAndSrc
    BN.MLIL_UNDEF -> return UNDEF
    BN.MLIL_UNIMPL -> return UNIMPL
    BN.MLIL_UNIMPL_MEM ->
        fmap UNIMPL_MEM $ UnimplMemOp <$> buildExpr
    BN.MLIL_FADD ->
        fmap FADD $ FaddOp <$> buildExpr <*> buildExpr
    BN.MLIL_FSUB ->
        fmap FSUB $ FsubOp <$> buildExpr <*> buildExpr
    BN.MLIL_FMUL ->
        fmap FMUL $ FmulOp <$> buildExpr <*> buildExpr
    BN.MLIL_FDIV ->
        fmap FDIV $ FdivOp <$> buildExpr <*> buildExpr
    BN.MLIL_FSQRT ->
        fmap FSQRT $ FsqrtOp <$> buildExpr
    BN.MLIL_FNEG ->
        fmap FNEG $ FnegOp <$> buildExpr
    BN.MLIL_FABS ->
        fmap FABS $ FabsOp <$> buildExpr
    BN.MLIL_FLOAT_TO_INT ->
        fmap FLOAT_TO_INT $ FloatToIntOp <$> buildExpr
    BN.MLIL_INT_TO_FLOAT ->
        fmap INT_TO_FLOAT $ IntToFloatOp <$> buildExpr
    BN.MLIL_FLOAT_CONV ->
        fmap FLOAT_CONV $ FloatConvOp <$> buildExpr
    BN.MLIL_ROUND_TO_INT ->
        fmap ROUND_TO_INT $ RoundToIntOp <$> buildExpr
    BN.MLIL_FLOOR ->
        fmap FLOOR $ FloorOp <$> buildExpr
    BN.MLIL_CEIL ->
        fmap CEIL $ CeilOp <$> buildExpr
    BN.MLIL_FTRUNC ->
        fmap FTRUNC $ FtruncOp <$> buildExpr
    BN.MLIL_FCMP_E ->
        fmap FCMP_E $ FcmpEOp <$> buildExpr <*> buildExpr
    BN.MLIL_FCMP_NE ->
        fmap FCMP_NE $ FcmpNeOp <$> buildExpr <*> buildExpr
    BN.MLIL_FCMP_LT ->
        fmap FCMP_LT $ FcmpLtOp <$> buildExpr <*> buildExpr
    BN.MLIL_FCMP_LE ->
        fmap FCMP_LE $ FcmpLeOp <$> buildExpr <*> buildExpr
    BN.MLIL_FCMP_GE ->
        fmap FCMP_GE $ FcmpGeOp <$> buildExpr <*> buildExpr
    BN.MLIL_FCMP_GT ->
        fmap FCMP_GT $ FcmpGtOp <$> buildExpr <*> buildExpr
    BN.MLIL_FCMP_O ->
        fmap FCMP_O $ FcmpOOp <$> buildExpr <*> buildExpr
    BN.MLIL_FCMP_UO ->
        fmap FCMP_UO $ FcmpUoOp <$> buildExpr <*> buildExpr
    BN.MLIL_SET_VAR_SSA ->
        fmap SET_VAR_SSA $ SetVarSSAOp <$> buildSSAVariable <*> buildExpr
    BN.MLIL_SET_VAR_SSA_FIELD ->
        fmap SET_VAR_SSA_FIELD $ SetVarSSAFieldOp <$> buildSSAVariableDestAndSrc <*> buildInt <*> buildExpr
    BN.MLIL_SET_VAR_SPLIT_SSA ->
        fmap SET_VAR_SPLIT_SSA $ SetVarSplitSSAOp <$> buildSSAVariable <*> buildSSAVariable <*> buildExpr
    BN.MLIL_SET_VAR_ALIASED ->
        fmap SET_VAR_ALIASED $ SetVarAliasedOp <$> buildSSAVariableDestAndSrc <*> buildExpr
    BN.MLIL_SET_VAR_ALIASED_FIELD ->
        fmap SET_VAR_ALIASED_FIELD $ SetVarAliasedFieldOp <$> buildSSAVariableDestAndSrc <*> buildInt <*> buildExpr
    BN.MLIL_VAR_SSA ->
        fmap VAR_SSA $ VarSSAOp <$> buildSSAVariable
    BN.MLIL_VAR_SSA_FIELD ->
        fmap VAR_SSA_FIELD $ VarSSAFieldOp <$> buildSSAVariable <*> buildInt
    BN.MLIL_VAR_ALIASED ->
        fmap VAR_ALIASED $ VarAliasedOp <$> buildSSAVariable
    BN.MLIL_VAR_ALIASED_FIELD ->
        fmap VAR_ALIASED_FIELD $ VarAliasedFieldOp <$> buildSSAVariable <*> buildInt
    BN.MLIL_VAR_SPLIT_SSA ->
        fmap VAR_SPLIT_SSA $ VarSplitSSAOp <$> buildSSAVariable <*> buildSSAVariable
    BN.MLIL_CALL_SSA ->
        fmap CALL_SSA $ CallSSAOp <$> buildExpr <*> buildExpr <*> buildExprList <*> buildInt
    BN.MLIL_CALL_UNTYPED_SSA ->
        fmap CALL_UNTYPED_SSA $ CallUntypedSSAOp <$> buildExpr <*> buildExpr <*> buildExpr <*> buildExpr
    BN.MLIL_SYSCALL_SSA ->
        fmap SYSCALL_SSA $ SyscallSSAOp <$> buildExpr <*> buildExprList <*> buildInt
    BN.MLIL_SYSCALL_UNTYPED_SSA ->
        fmap SYSCALL_UNTYPED_SSA $ SyscallUntypedSSAOp <$> buildExpr <*> buildExpr <*> buildExpr
    BN.MLIL_TAILCALL_SSA ->
        fmap TAILCALL_SSA $ TailcallSSAOp <$> buildExpr <*> buildExpr <*> buildExprList <*> buildInt
    BN.MLIL_TAILCALL_UNTYPED_SSA ->
        fmap TAILCALL_UNTYPED_SSA $ TailcallUntypedSSAOp <$> buildExpr <*> buildExpr <*> buildExpr <*> buildExpr
    BN.MLIL_CALL_OUTPUT_SSA ->
        fmap CALL_OUTPUT_SSA $ CallOutputSSAOp <$> buildInt <*> buildSSAVarList
    BN.MLIL_CALL_PARAM_SSA ->
        fmap CALL_PARAM_SSA $ CallParamSSAOp <$> buildInt <*> buildSSAVarList
    BN.MLIL_LOAD_SSA ->
        fmap LOAD_SSA $ LoadSSAOp <$> buildExpr <*> buildInt
    BN.MLIL_LOAD_STRUCT_SSA ->
        fmap LOAD_STRUCT_SSA $ LoadStructSSAOp <$> buildExpr <*> buildInt <*> buildInt
    BN.MLIL_STORE_SSA ->
        fmap STORE_SSA $ StoreSSAOp <$> buildExpr <*> buildInt <*> buildInt <*> buildExpr
    BN.MLIL_STORE_STRUCT_SSA ->
        fmap STORE_STRUCT_SSA $ StoreStructSSAOp <$> buildExpr <*> buildInt <*> buildInt <*> buildInt <*> buildExpr
    BN.MLIL_VAR_PHI ->
        fmap VAR_PHI $ VarPhiOp <$> buildSSAVariable <*> buildSSAVarList
    BN.MLIL_MEM_PHI ->
        fmap MEM_PHI $ MemPhiOp <$> buildInt <*> buildIntList

-- TODO: the params returning single expr can probably be expanded
getParams :: Operation (Expression MLILSSAFunction) -> [Expression MLILSSAFunction]
getParams (CALL op') = op' ^. params
getParams (CALL_SSA op') = op' ^. params
getParams (CALL_UNTYPED op') = [op' ^. params]
getParams (CALL_UNTYPED_SSA op') = [op' ^. params]
getParams (TAILCALL op') = op' ^. params
getParams (TAILCALL_SSA op') = op' ^. params
getParams (TAILCALL_UNTYPED op') = [op' ^. params]
getParams (TAILCALL_UNTYPED_SSA op') = [op' ^. params]
getParams (SYSCALL op') = op' ^. params
getParams (SYSCALL_SSA op') = op' ^. params
getParams (SYSCALL_UNTYPED op') = [op' ^. params]
getParams (SYSCALL_UNTYPED_SSA op') = [op' ^. params]
getParams _ = []
