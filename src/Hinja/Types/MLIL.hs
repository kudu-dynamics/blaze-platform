{-# LANGUAGE TemplateHaskell #-}

module Hinja.Types.MLIL
  ( module Exports
  , module Hinja.Types.MLIL
  ) where

import Hinja.Prelude

import Hinja.C.Enums
import Hinja.C.Types
import Hinja.Types.MLIL.Common as Exports
import Hinja.Types.MLIL.Ops as Exports
import qualified Prelude as P

newtype OpIndex = OpIndex Word64
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype OperationSize = OperationSize Word64
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype OperandsData = OperandsData [Word64]
  deriving (Eq, Ord, Show)

data MediumLevelILInstruction = MediumLevelILInstruction
  { _operation :: BNMediumLevelILOperation
  , _sourceOperand :: Bool
  , _size :: OperationSize
  , _operands :: OperandsData
  , _address :: Address
  } deriving (Eq, Ord, Show)

data OpBuilderCtx fun = OpBuilderCtx
  { _func :: fun
  , _exprIndex :: ExpressionIndex fun
  , _opData :: OperandsData
  , _size :: OperationSize
  } deriving (Eq, Ord, Show)

newtype OpBuilder fun a = OpBuilder
  { runOpBuilder_ :: ReaderT (OpBuilderCtx fun) (StateT OpIndex IO) a }
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadReader (OpBuilderCtx fun)
           , MonadState OpIndex)

runOpBuilder :: OpBuilder t a -> (OpBuilderCtx t, OpIndex) -> IO a
runOpBuilder m (ctx, s) = flip evalStateT s . flip runReaderT ctx . runOpBuilder_ $ m

-- takes first op data long and reduces state
-- crashes if op-data is empty -- should never happen
takeOpDataWord :: OpBuilder t Word64
takeOpDataWord = do
  (OpIndex n) <- get
  if n >= 5
    then P.error "takeOpData: exhausted OperandsData"
    else do
      (OperandsData xs) <- _opData <$> ask
      modify (+1)
      return $ xs !! fromIntegral n

getAndAdvanceOpIndex :: OpBuilder t OpIndex
getAndAdvanceOpIndex = do
  n <- get
  modify (+1)
  return n

data Instruction t = Instruction
  { _address :: Address
  , _index :: InstructionIndex t
  , _size :: OperationSize
  , _op :: Operation (Expression t)
  } deriving (Eq, Ord, Show)

data Expression t = Expression
  { _address :: Address
  , _index :: ExpressionIndex t
  , _size :: OperationSize
  , _op :: Operation (Expression t)
  } deriving (Eq, Ord, Show)


data Operation expr
    = NOP
    | SET_VAR (SetVarOp expr)
    | SET_VAR_FIELD (SetVarFieldOp expr)
    | SET_VAR_SPLIT (SetVarSplitOp expr)
    | LOAD (LoadOp expr)
    | LOAD_STRUCT (LoadStructOp expr)
    | STORE (StoreOp expr)
    | STORE_STRUCT (StoreStructOp expr)
    | VAR (VarOp expr)
    | VAR_FIELD (VarFieldOp expr)
    | VAR_SPLIT (VarSplitOp expr)
    | ADDRESS_OF (AddressOfOp expr)
    | ADDRESS_OF_FIELD (AddressOfFieldOp expr)
    | CONST (ConstOp expr)
    | CONST_PTR (ConstPtrOp expr)
    | EXTERN_PTR (ExternPtrOp expr)
    | FLOAT_CONST (FloatConstOp expr)
    | IMPORT (ImportOp expr)
    | ADD (AddOp expr)
    | ADC (AdcOp expr)
    | SUB (SubOp expr)
    | SBB (SbbOp expr)
    | AND (AndOp expr)
    | OR (OrOp expr)
    | XOR (XorOp expr)
    | LSL (LslOp expr)
    | LSR (LsrOp expr)
    | ASR (AsrOp expr)
    | ROL (RolOp expr)
    | RLC (RlcOp expr)
    | ROR (RorOp expr)
    | RRC (RrcOp expr)
    | MUL (MulOp expr)
    | MULU_DP (MuluDpOp expr)
    | MULS_DP (MulsDpOp expr)
    | DIVU (DivuOp expr)
    | DIVU_DP (DivuDpOp expr)
    | DIVS (DivsOp expr)
    | DIVS_DP (DivsDpOp expr)
    | MODU (ModuOp expr)
    | MODU_DP (ModuDpOp expr)
    | MODS (ModsOp expr)
    | MODS_DP (ModsDpOp expr)
    | NEG (NegOp expr)
    | NOT (NotOp expr)
    | SX (SxOp expr)
    | ZX (ZxOp expr)
    | LOW_PART (LowPartOp expr)
    | JUMP (JumpOp expr)
    | JUMP_TO (JumpToOp expr)
    | RET_HINT (RetHintOp expr)
    | CALL (CallOp expr)
    | CALL_UNTYPED (CallUntypedOp expr)
    | CALL_OUTPUT (CallOutputOp expr)
    | CALL_PARAM (CallParamOp expr)
    | RET (RetOp expr)
    | NORET
    | IF (IfOp expr)
    | GOTO (GotoOp expr)
    | CMP_E (CmpEOp expr)
    | CMP_NE (CmpNeOp expr)
    | CMP_SLT (CmpSltOp expr)
    | CMP_ULT (CmpUltOp expr)
    | CMP_SLE (CmpSleOp expr)
    | CMP_ULE (CmpUleOp expr)
    | CMP_SGE (CmpSgeOp expr)
    | CMP_UGE (CmpUgeOp expr)
    | CMP_SGT (CmpSgtOp expr)
    | CMP_UGT (CmpUgtOp expr)
    | TEST_BIT (TestBitOp expr)
    | BOOL_TO_INT (BoolToIntOp expr)
    | ADD_OVERFLOW (AddOverflowOp expr)
    | SYSCALL (SyscallOp expr)
    | SYSCALL_UNTYPED (SyscallUntypedOp expr)
    | TAILCALL (TailcallOp expr)
    | TAILCALL_UNTYPED (TailcallUntypedOp expr)
    | BP
    | TRAP (TrapOp expr)
    | INTRINSIC (IntrinsicOp expr)
    | INTRINSIC_SSA (IntrinsicSSAOp expr)
    | FREE_VAR_SLOT (FreeVarSlotOp expr)
    | FREE_VAR_SLOT_SSA (FreeVarSlotSSAOp expr)
    | UNDEF
    | UNIMPL
    | UNIMPL_MEM (UnimplMemOp expr)
    | FADD (FaddOp expr)
    | FSUB (FsubOp expr)
    | FMUL (FmulOp expr)
    | FDIV (FdivOp expr)
    | FSQRT (FsqrtOp expr)
    | FNEG (FnegOp expr)
    | FABS (FabsOp expr)
    | FLOAT_TO_INT (FloatToIntOp expr)
    | INT_TO_FLOAT (IntToFloatOp expr)
    | FLOAT_CONV (FloatConvOp expr)
    | ROUND_TO_INT (RoundToIntOp expr)
    | FLOOR (FloorOp expr)
    | CEIL (CeilOp expr)
    | FTRUNC (FtruncOp expr)
    | FCMP_E (FcmpEOp expr)
    | FCMP_NE (FcmpNeOp expr)
    | FCMP_LT (FcmpLtOp expr)
    | FCMP_LE (FcmpLeOp expr)
    | FCMP_GE (FcmpGeOp expr)
    | FCMP_GT (FcmpGtOp expr)
    | FCMP_O (FcmpOOp expr)
    | FCMP_UO (FcmpUoOp expr)
    | SET_VAR_SSA (SetVarSSAOp expr)
    | SET_VAR_SSA_FIELD (SetVarSSAFieldOp expr)
    | SET_VAR_SPLIT_SSA (SetVarSplitSSAOp expr)
    | SET_VAR_ALIASED (SetVarAliasedOp expr)
    | SET_VAR_ALIASED_FIELD (SetVarAliasedFieldOp expr)
    | VAR_SSA (VarSSAOp expr)
    | VAR_SSA_FIELD (VarSSAFieldOp expr)
    | VAR_ALIASED (VarAliasedOp expr)
    | VAR_ALIASED_FIELD (VarAliasedFieldOp expr)
    | VAR_SPLIT_SSA (VarSplitSSAOp expr)
    | CALL_SSA (CallSSAOp expr)
    | CALL_UNTYPED_SSA (CallUntypedSSAOp expr)
    | SYSCALL_SSA (SyscallSSAOp expr)
    | SYSCALL_UNTYPED_SSA (SyscallUntypedSSAOp expr)
    | TAILCALL_SSA (TailcallSSAOp expr)
    | TAILCALL_UNTYPED_SSA (TailcallUntypedSSAOp expr)
    | CALL_OUTPUT_SSA (CallOutputSSAOp expr)
    | CALL_PARAM_SSA (CallParamSSAOp expr)
    | LOAD_SSA (LoadSSAOp expr)
    | LOAD_STRUCT_SSA (LoadStructSSAOp expr)
    | STORE_SSA (StoreSSAOp expr)
    | STORE_STRUCT_SSA (StoreStructSSAOp expr)
    | VAR_PHI (VarPhiOp expr)
    | MEM_PHI (MemPhiOp expr)
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)


$(makeFields ''NopOp)
$(makeFields ''SetVarOp)
$(makeFields ''SetVarFieldOp)
$(makeFields ''SetVarSplitOp)
$(makeFields ''LoadOp)
$(makeFields ''LoadStructOp)
$(makeFields ''StoreOp)
$(makeFields ''StoreStructOp)
$(makeFields ''VarOp)
$(makeFields ''VarFieldOp)
$(makeFields ''VarSplitOp)
$(makeFields ''AddressOfOp)
$(makeFields ''AddressOfFieldOp)
$(makeFields ''ConstOp)
$(makeFields ''ConstPtrOp)
$(makeFields ''ExternPtrOp)
$(makeFields ''FloatConstOp)
$(makeFields ''ImportOp)
$(makeFields ''AddOp)
$(makeFields ''AdcOp)
$(makeFields ''SubOp)
$(makeFields ''SbbOp)
$(makeFields ''AndOp)
$(makeFields ''OrOp)
$(makeFields ''XorOp)
$(makeFields ''LslOp)
$(makeFields ''LsrOp)
$(makeFields ''AsrOp)
$(makeFields ''RolOp)
$(makeFields ''RlcOp)
$(makeFields ''RorOp)
$(makeFields ''RrcOp)
$(makeFields ''MulOp)
$(makeFields ''MuluDpOp)
$(makeFields ''MulsDpOp)
$(makeFields ''DivuOp)
$(makeFields ''DivuDpOp)
$(makeFields ''DivsOp)
$(makeFields ''DivsDpOp)
$(makeFields ''ModuOp)
$(makeFields ''ModuDpOp)
$(makeFields ''ModsOp)
$(makeFields ''ModsDpOp)
$(makeFields ''NegOp)
$(makeFields ''NotOp)
$(makeFields ''SxOp)
$(makeFields ''ZxOp)
$(makeFields ''LowPartOp)
$(makeFields ''JumpOp)
$(makeFields ''JumpToOp)
$(makeFields ''RetHintOp)
$(makeFields ''CallOp)
$(makeFields ''CallUntypedOp)
$(makeFields ''CallOutputOp)
$(makeFields ''CallParamOp)
$(makeFields ''RetOp)
$(makeFields ''NoretOp)
$(makeFields ''IfOp)
$(makeFields ''GotoOp)
$(makeFields ''CmpEOp)
$(makeFields ''CmpNeOp)
$(makeFields ''CmpSltOp)
$(makeFields ''CmpUltOp)
$(makeFields ''CmpSleOp)
$(makeFields ''CmpUleOp)
$(makeFields ''CmpSgeOp)
$(makeFields ''CmpUgeOp)
$(makeFields ''CmpSgtOp)
$(makeFields ''CmpUgtOp)
$(makeFields ''TestBitOp)
$(makeFields ''BoolToIntOp)
$(makeFields ''AddOverflowOp)
$(makeFields ''SyscallOp)
$(makeFields ''SyscallUntypedOp)
$(makeFields ''TailcallOp)
$(makeFields ''TailcallUntypedOp)
$(makeFields ''BpOp)
$(makeFields ''TrapOp)
$(makeFields ''IntrinsicOp)
$(makeFields ''IntrinsicSSAOp)
$(makeFields ''FreeVarSlotOp)
$(makeFields ''FreeVarSlotSSAOp)
$(makeFields ''UndefOp)
$(makeFields ''UnimplOp)
$(makeFields ''UnimplMemOp)
$(makeFields ''FaddOp)
$(makeFields ''FsubOp)
$(makeFields ''FmulOp)
$(makeFields ''FdivOp)
$(makeFields ''FsqrtOp)
$(makeFields ''FnegOp)
$(makeFields ''FabsOp)
$(makeFields ''FloatToIntOp)
$(makeFields ''IntToFloatOp)
$(makeFields ''FloatConvOp)
$(makeFields ''RoundToIntOp)
$(makeFields ''FloorOp)
$(makeFields ''CeilOp)
$(makeFields ''FtruncOp)
$(makeFields ''FcmpEOp)
$(makeFields ''FcmpNeOp)
$(makeFields ''FcmpLtOp)
$(makeFields ''FcmpLeOp)
$(makeFields ''FcmpGeOp)
$(makeFields ''FcmpGtOp)
$(makeFields ''FcmpOOp)
$(makeFields ''FcmpUoOp)
$(makeFields ''SetVarSSAOp)
$(makeFields ''SetVarSSAFieldOp)
$(makeFields ''SetVarSplitSSAOp)
$(makeFields ''SetVarAliasedOp)
$(makeFields ''SetVarAliasedFieldOp)
$(makeFields ''VarSSAOp)
$(makeFields ''VarSSAFieldOp)
$(makeFields ''VarAliasedOp)
$(makeFields ''VarAliasedFieldOp)
$(makeFields ''VarSplitSSAOp)
$(makeFields ''CallSSAOp)
$(makeFields ''CallUntypedSSAOp)
$(makeFields ''SyscallSSAOp)
$(makeFields ''SyscallUntypedSSAOp)
$(makeFields ''TailcallSSAOp)
$(makeFields ''TailcallUntypedSSAOp)
$(makeFields ''CallOutputSSAOp)
$(makeFields ''CallParamSSAOp)
$(makeFields ''LoadSSAOp)
$(makeFields ''LoadStructSSAOp)
$(makeFields ''StoreSSAOp)
$(makeFields ''StoreStructSSAOp)
$(makeFields ''VarPhiOp)
$(makeFields ''MemPhiOp)


$(makeFieldsNoPrefix ''OpBuilderCtx)
$(makeFieldsNoPrefix ''Expression)
$(makeFieldsNoPrefix ''Instruction)
$(makeFieldsNoPrefix ''SSAVariable)
$(makeFieldsNoPrefix ''SSAVariableDestAndSrc)
$(makeFieldsNoPrefix ''MediumLevelILInstruction)
