{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Hinja.MLIL.Types where

import Hinja.Prelude hiding (onException, handle)

import qualified Prelude as P
import Hinja.Types
import Hinja.C.Enums
import Hinja.C.Types
import Hinja.C.Pointers

data MediumLevelILInstruction = MediumLevelILInstruction
  { _operation :: BNMediumLevelILOperation
  , _sourceOperand :: Bool
  , _size :: OperationSize
  , _operands :: OperandsData
  , _address :: Address
  } deriving (Eq, Ord, Show)

newtype Intrinsic = Intrinsic Word64
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype OpIndex = OpIndex Word64
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype OperationSize = OperationSize Word64
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype OperandsData = OperandsData [Word64]
  deriving (Eq, Ord, Show)

data OpBuilderCtx fun = OpBuilderCtx
  { _func :: fun
  , _exprIndex :: ExpressionIndex fun
  , _opData :: OperandsData
  , _size :: OperationSize
  } deriving (Eq, Ord, Show)

class ( HasHandle fun BNMediumLevelILFunction
      , HasFunc fun Function ) => StatementFunction fun where
  getExprIndex :: fun -> InstructionIndex fun -> IO (ExpressionIndex fun)

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
  , _op :: Operation t
  } deriving (Eq, Ord, Show)

data Expression t = Expression
  { _address :: Address
  , _index :: ExpressionIndex t
  , _size :: OperationSize
  , _op :: Operation t
  } deriving (Eq, Ord, Show)


-- data Operation t
--   = NOP
--   | SET_VAR (SetVarOp t)
--   | SET_VAR_FIELD (SetVarFieldOp t)
--   | SET_VAR_SSA (SetVarSSAOp t)
--   deriving (Eq, Ord, Show)

data SSAVariable = SSAVariable { _var :: Variable
                               , _version :: Int
                               } deriving (Eq, Ord, Show)

data SSAVariableDestAndSrc = SSAVariableDestAndSrc
  { _dest :: SSAVariable
  , _src :: SSAVariable
  } deriving (Eq, Ord, Show)

-- data SetVarSSAOp t = SetVarSSAOp { _dest :: SSAVariable
--                                  , _src  :: Expression t
--                                  } deriving (Eq, Ord, Show)

-- data SetVarOp t = SetVarOp
--   { _dest :: Variable
--   , _src :: Expression t
--   } deriving (Eq, Ord, Show)

-- data SetVarFieldOp t = SetVarFieldOp { _dest :: Variable
--                                      , _offset :: Int
--                                      , _src :: Expression t
--                                      } deriving (Eq, Ord, Show)

----------------------

data Operation t
    = NOP
    | SET_VAR (SetVarOp t)
    | SET_VAR_FIELD (SetVarFieldOp t)
    | SET_VAR_SPLIT (SetVarSplitOp t)
    | LOAD (LoadOp t)
    | LOAD_STRUCT (LoadStructOp t)
    | STORE (StoreOp t)
    | STORE_STRUCT (StoreStructOp t)
    | VAR (VarOp t)
    | VAR_FIELD (VarFieldOp t)
    | VAR_SPLIT (VarSplitOp t)
    | ADDRESS_OF (AddressOfOp t)
    | ADDRESS_OF_FIELD (AddressOfFieldOp t)
    | CONST (ConstOp t)
    | CONST_PTR (ConstPtrOp t)
    | EXTERN_PTR (ExternPtrOp t)
    | FLOAT_CONST (FloatConstOp t)
    | IMPORT (ImportOp t)
    | ADD (AddOp t)
    | ADC (AdcOp t)
    | SUB (SubOp t)
    | SBB (SbbOp t)
    | AND (AndOp t)
    | OR (OrOp t)
    | XOR (XorOp t)
    | LSL (LslOp t)
    | LSR (LsrOp t)
    | ASR (AsrOp t)
    | ROL (RolOp t)
    | RLC (RlcOp t)
    | ROR (RorOp t)
    | RRC (RrcOp t)
    | MUL (MulOp t)
    | MULU_DP (MuluDpOp t)
    | MULS_DP (MulsDpOp t)
    | DIVU (DivuOp t)
    | DIVU_DP (DivuDpOp t)
    | DIVS (DivsOp t)
    | DIVS_DP (DivsDpOp t)
    | MODU (ModuOp t)
    | MODU_DP (ModuDpOp t)
    | MODS (ModsOp t)
    | MODS_DP (ModsDpOp t)
    | NEG (NegOp t)
    | NOT (NotOp t)
    | SX (SxOp t)
    | ZX (ZxOp t)
    | LOW_PART (LowPartOp t)
    | JUMP (JumpOp t)
    | JUMP_TO (JumpToOp t)
    | RET_HINT (RetHintOp t)
    | CALL (CallOp t)
    | CALL_UNTYPED (CallUntypedOp t)
    | CALL_OUTPUT (CallOutputOp t)
    | CALL_PARAM (CallParamOp t)
    | RET (RetOp t)
    | NORET
    | IF (IfOp t)
    | GOTO (GotoOp t)
    | CMP_E (CmpEOp t)
    | CMP_NE (CmpNeOp t)
    | CMP_SLT (CmpSltOp t)
    | CMP_ULT (CmpUltOp t)
    | CMP_SLE (CmpSleOp t)
    | CMP_ULE (CmpUleOp t)
    | CMP_SGE (CmpSgeOp t)
    | CMP_UGE (CmpUgeOp t)
    | CMP_SGT (CmpSgtOp t)
    | CMP_UGT (CmpUgtOp t)
    | TEST_BIT (TestBitOp t)
    | BOOL_TO_INT (BoolToIntOp t)
    | ADD_OVERFLOW (AddOverflowOp t)
    | SYSCALL (SyscallOp t)
    | SYSCALL_UNTYPED (SyscallUntypedOp t)
    | TAILCALL (TailcallOp t)
    | TAILCALL_UNTYPED (TailcallUntypedOp t)
    | BP
    | TRAP (TrapOp t)
    | INTRINSIC (IntrinsicOp t)
    | INTRINSIC_SSA (IntrinsicSSAOp t)
    | FREE_VAR_SLOT (FreeVarSlotOp t)
    | FREE_VAR_SLOT_SSA (FreeVarSlotSSAOp t)
    | UNDEF
    | UNIMPL
    | UNIMPL_MEM (UnimplMemOp t)
    | FADD (FaddOp t)
    | FSUB (FsubOp t)
    | FMUL (FmulOp t)
    | FDIV (FdivOp t)
    | FSQRT (FsqrtOp t)
    | FNEG (FnegOp t)
    | FABS (FabsOp t)
    | FLOAT_TO_INT (FloatToIntOp t)
    | INT_TO_FLOAT (IntToFloatOp t)
    | FLOAT_CONV (FloatConvOp t)
    | ROUND_TO_INT (RoundToIntOp t)
    | FLOOR (FloorOp t)
    | CEIL (CeilOp t)
    | FTRUNC (FtruncOp t)
    | FCMP_E (FcmpEOp t)
    | FCMP_NE (FcmpNeOp t)
    | FCMP_LT (FcmpLtOp t)
    | FCMP_LE (FcmpLeOp t)
    | FCMP_GE (FcmpGeOp t)
    | FCMP_GT (FcmpGtOp t)
    | FCMP_O (FcmpOOp t)
    | FCMP_UO (FcmpUoOp t)
    | SET_VAR_SSA (SetVarSSAOp t)
    | SET_VAR_SSA_FIELD (SetVarSSAFieldOp t)
    | SET_VAR_SPLIT_SSA (SetVarSplitSSAOp t)
    | SET_VAR_ALIASED (SetVarAliasedOp t)
    | SET_VAR_ALIASED_FIELD (SetVarAliasedFieldOp t)
    | VAR_SSA (VarSSAOp t)
    | VAR_SSA_FIELD (VarSSAFieldOp t)
    | VAR_ALIASED (VarAliasedOp t)
    | VAR_ALIASED_FIELD (VarAliasedFieldOp t)
    | VAR_SPLIT_SSA (VarSplitSSAOp t)
    | CALL_SSA (CallSSAOp t)
    | CALL_UNTYPED_SSA (CallUntypedSSAOp t)
    | SYSCALL_SSA (SyscallSSAOp t)
    | SYSCALL_UNTYPED_SSA (SyscallUntypedSSAOp t)
    | TAILCALL_SSA (TailcallSSAOp t)
    | TAILCALL_UNTYPED_SSA (TailcallUntypedSSAOp t)
    | CALL_OUTPUT_SSA (CallOutputSSAOp t)
    | CALL_PARAM_SSA (CallParamSSAOp t)
    | LOAD_SSA (LoadSSAOp t)
    | LOAD_STRUCT_SSA (LoadStructSSAOp t)
    | STORE_SSA (StoreSSAOp t)
    | STORE_STRUCT_SSA (StoreStructSSAOp t)
    | VAR_PHI (VarPhiOp t)
    | MEM_PHI (MemPhiOp t)
    deriving (Eq, Ord, Show)

----------------------

data NopOp t = NopOp
    deriving (Eq, Ord, Show)

data SetVarOp t = SetVarOp
    { _dest :: Variable
    , _src :: Expression t
    } deriving (Eq, Ord, Show)

data SetVarFieldOp t = SetVarFieldOp
    { _dest :: Variable
    , _offset :: Int64
    , _src :: Expression t
    } deriving (Eq, Ord, Show)

data SetVarSplitOp t = SetVarSplitOp
    { _high :: Variable
    , _low :: Variable
    , _src :: Expression t
    } deriving (Eq, Ord, Show)

data LoadOp t = LoadOp
    { _src :: Expression t
    } deriving (Eq, Ord, Show)

data LoadStructOp t = LoadStructOp
    { _src :: Expression t
    , _offset :: Int64
    } deriving (Eq, Ord, Show)

data StoreOp t = StoreOp
    { _dest :: Expression t
    , _src :: Expression t
    } deriving (Eq, Ord, Show)

data StoreStructOp t = StoreStructOp
    { _dest :: Expression t
    , _offset :: Int64
    , _src :: Expression t
    } deriving (Eq, Ord, Show)

data VarOp t = VarOp
    { _src :: Variable
    } deriving (Eq, Ord, Show)

data VarFieldOp t = VarFieldOp
    { _src :: Variable
    , _offset :: Int64
    } deriving (Eq, Ord, Show)

data VarSplitOp t = VarSplitOp
    { _high :: Variable
    , _low :: Variable
    } deriving (Eq, Ord, Show)

data AddressOfOp t = AddressOfOp
    { _src :: Variable
    } deriving (Eq, Ord, Show)

data AddressOfFieldOp t = AddressOfFieldOp
    { _src :: Variable
    , _offset :: Int64
    } deriving (Eq, Ord, Show)

data ConstOp t = ConstOp
    { _constant :: Int64
    } deriving (Eq, Ord, Show)

data ConstPtrOp t = ConstPtrOp
    { _constant :: Int64
    } deriving (Eq, Ord, Show)

data ExternPtrOp t = ExternPtrOp
    { _constant :: Int64
    , _offset :: Int64
    } deriving (Eq, Ord, Show)

data FloatConstOp t = FloatConstOp
    { _constant :: Double
    } deriving (Eq, Ord, Show)

data ImportOp t = ImportOp
    { _constant :: Int64
    } deriving (Eq, Ord, Show)

data AddOp t = AddOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data AdcOp t = AdcOp
    { _left :: Expression t
    , _right :: Expression t
    , _carry :: Expression t
    } deriving (Eq, Ord, Show)

data SubOp t = SubOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data SbbOp t = SbbOp
    { _left :: Expression t
    , _right :: Expression t
    , _carry :: Expression t
    } deriving (Eq, Ord, Show)

data AndOp t = AndOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data OrOp t = OrOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data XorOp t = XorOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data LslOp t = LslOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data LsrOp t = LsrOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data AsrOp t = AsrOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data RolOp t = RolOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data RlcOp t = RlcOp
    { _left :: Expression t
    , _right :: Expression t
    , _carry :: Expression t
    } deriving (Eq, Ord, Show)

data RorOp t = RorOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data RrcOp t = RrcOp
    { _left :: Expression t
    , _right :: Expression t
    , _carry :: Expression t
    } deriving (Eq, Ord, Show)

data MulOp t = MulOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data MuluDpOp t = MuluDpOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data MulsDpOp t = MulsDpOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data DivuOp t = DivuOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data DivuDpOp t = DivuDpOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data DivsOp t = DivsOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data DivsDpOp t = DivsDpOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data ModuOp t = ModuOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data ModuDpOp t = ModuDpOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data ModsOp t = ModsOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data ModsDpOp t = ModsDpOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data NegOp t = NegOp
    { _src :: Expression t
    } deriving (Eq, Ord, Show)

data NotOp t = NotOp
    { _src :: Expression t
    } deriving (Eq, Ord, Show)

data SxOp t = SxOp
    { _src :: Expression t
    } deriving (Eq, Ord, Show)

data ZxOp t = ZxOp
    { _src :: Expression t
    } deriving (Eq, Ord, Show)

data LowPartOp t = LowPartOp
    { _src :: Expression t
    } deriving (Eq, Ord, Show)

data JumpOp t = JumpOp
    { _dest :: Expression t
    } deriving (Eq, Ord, Show)

data JumpToOp t = JumpToOp
    { _dest :: Expression t
    , _targets :: [Int64]
    } deriving (Eq, Ord, Show)

data RetHintOp t = RetHintOp
    { _dest :: Expression t
    } deriving (Eq, Ord, Show)

data CallOp t = CallOp
    { _output :: [Variable]
    , _dest :: Expression t
    , _params :: [Expression t]
    } deriving (Eq, Ord, Show)

data CallUntypedOp t = CallUntypedOp
    { _output :: Expression t
    , _dest :: Expression t
    , _params :: Expression t
    , _stack :: Expression t
    } deriving (Eq, Ord, Show)

data CallOutputOp t = CallOutputOp
    { _dest :: [Variable]
    } deriving (Eq, Ord, Show)

data CallParamOp t = CallParamOp
    { _src :: [Variable]
    } deriving (Eq, Ord, Show)

data RetOp t = RetOp
    { _src :: [Expression t]
    } deriving (Eq, Ord, Show)

data NoretOp t = NoretOp
    deriving (Eq, Ord, Show)

data IfOp t = IfOp
    { _condition :: Expression t
    , _true :: Int64
    , _false :: Int64
    } deriving (Eq, Ord, Show)

data GotoOp t = GotoOp
    { _dest :: Int64
    } deriving (Eq, Ord, Show)

data CmpEOp t = CmpEOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data CmpNeOp t = CmpNeOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data CmpSltOp t = CmpSltOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data CmpUltOp t = CmpUltOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data CmpSleOp t = CmpSleOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data CmpUleOp t = CmpUleOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data CmpSgeOp t = CmpSgeOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data CmpUgeOp t = CmpUgeOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data CmpSgtOp t = CmpSgtOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data CmpUgtOp t = CmpUgtOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data TestBitOp t = TestBitOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data BoolToIntOp t = BoolToIntOp
    { _src :: Expression t
    } deriving (Eq, Ord, Show)

data AddOverflowOp t = AddOverflowOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data SyscallOp t = SyscallOp
    { _output :: [Variable]
    , _params :: [Expression t]
    } deriving (Eq, Ord, Show)

data SyscallUntypedOp t = SyscallUntypedOp
    { _output :: Expression t
    , _params :: Expression t
    , _stack :: Expression t
    } deriving (Eq, Ord, Show)

data TailcallOp t = TailcallOp
    { _output :: [Variable]
    , _dest :: Expression t
    , _params :: [Expression t]
    } deriving (Eq, Ord, Show)

data TailcallUntypedOp t = TailcallUntypedOp
    { _output :: Expression t
    , _dest :: Expression t
    , _params :: Expression t
    , _stack :: Expression t
    } deriving (Eq, Ord, Show)

data BpOp t = BpOp
    deriving (Eq, Ord, Show)

data TrapOp t = TrapOp
    { _vector :: Int64
    } deriving (Eq, Ord, Show)

data IntrinsicOp t = IntrinsicOp
    { _output :: [Variable]
    , _intrinsic :: Intrinsic
    , _params :: [Expression t]
    } deriving (Eq, Ord, Show)

data IntrinsicSSAOp t = IntrinsicSSAOp
    { _output :: [SSAVariable]
    , _intrinsic :: Intrinsic
    , _params :: [Expression t]
    } deriving (Eq, Ord, Show)

data FreeVarSlotOp t = FreeVarSlotOp
    { _dest :: Variable
    } deriving (Eq, Ord, Show)

data FreeVarSlotSSAOp t = FreeVarSlotSSAOp
    { _prev :: SSAVariableDestAndSrc
    } deriving (Eq, Ord, Show)

data UndefOp t = UndefOp
    deriving (Eq, Ord, Show)

data UnimplOp t = UnimplOp
    deriving (Eq, Ord, Show)

data UnimplMemOp t = UnimplMemOp
    { _src :: Expression t
    } deriving (Eq, Ord, Show)

data FaddOp t = FaddOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data FsubOp t = FsubOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data FmulOp t = FmulOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data FdivOp t = FdivOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data FsqrtOp t = FsqrtOp
    { _src :: Expression t
    } deriving (Eq, Ord, Show)

data FnegOp t = FnegOp
    { _src :: Expression t
    } deriving (Eq, Ord, Show)

data FabsOp t = FabsOp
    { _src :: Expression t
    } deriving (Eq, Ord, Show)

data FloatToIntOp t = FloatToIntOp
    { _src :: Expression t
    } deriving (Eq, Ord, Show)

data IntToFloatOp t = IntToFloatOp
    { _src :: Expression t
    } deriving (Eq, Ord, Show)

data FloatConvOp t = FloatConvOp
    { _src :: Expression t
    } deriving (Eq, Ord, Show)

data RoundToIntOp t = RoundToIntOp
    { _src :: Expression t
    } deriving (Eq, Ord, Show)

data FloorOp t = FloorOp
    { _src :: Expression t
    } deriving (Eq, Ord, Show)

data CeilOp t = CeilOp
    { _src :: Expression t
    } deriving (Eq, Ord, Show)

data FtruncOp t = FtruncOp
    { _src :: Expression t
    } deriving (Eq, Ord, Show)

data FcmpEOp t = FcmpEOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data FcmpNeOp t = FcmpNeOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data FcmpLtOp t = FcmpLtOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data FcmpLeOp t = FcmpLeOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data FcmpGeOp t = FcmpGeOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data FcmpGtOp t = FcmpGtOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data FcmpOOp t = FcmpOOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data FcmpUoOp t = FcmpUoOp
    { _left :: Expression t
    , _right :: Expression t
    } deriving (Eq, Ord, Show)

data SetVarSSAOp t = SetVarSSAOp
    { _dest :: SSAVariable
    , _src :: Expression t
    } deriving (Eq, Ord, Show)

data SetVarSSAFieldOp t = SetVarSSAFieldOp
    { _prev :: SSAVariableDestAndSrc
    , _offset :: Int64
    , _src :: Expression t
    } deriving (Eq, Ord, Show)

data SetVarSplitSSAOp t = SetVarSplitSSAOp
    { _high :: SSAVariable
    , _low :: SSAVariable
    , _src :: Expression t
    } deriving (Eq, Ord, Show)

data SetVarAliasedOp t = SetVarAliasedOp
    { _prev :: SSAVariableDestAndSrc
    , _src :: Expression t
    } deriving (Eq, Ord, Show)

data SetVarAliasedFieldOp t = SetVarAliasedFieldOp
    { _prev :: SSAVariableDestAndSrc
    , _offset :: Int64
    , _src :: Expression t
    } deriving (Eq, Ord, Show)

data VarSSAOp t = VarSSAOp
    { _src :: SSAVariable
    } deriving (Eq, Ord, Show)

data VarSSAFieldOp t = VarSSAFieldOp
    { _src :: SSAVariable
    , _offset :: Int64
    } deriving (Eq, Ord, Show)

data VarAliasedOp t = VarAliasedOp
    { _src :: SSAVariable
    } deriving (Eq, Ord, Show)

data VarAliasedFieldOp t = VarAliasedFieldOp
    { _src :: SSAVariable
    , _offset :: Int64
    } deriving (Eq, Ord, Show)

data VarSplitSSAOp t = VarSplitSSAOp
    { _high :: SSAVariable
    , _low :: SSAVariable
    } deriving (Eq, Ord, Show)

data CallSSAOp t = CallSSAOp
    { _output :: Expression t
    , _dest :: Expression t
    , _params :: [Expression t]
    , _src_memory :: Int64
    } deriving (Eq, Ord, Show)

data CallUntypedSSAOp t = CallUntypedSSAOp
    { _output :: Expression t
    , _dest :: Expression t
    , _params :: Expression t
    , _stack :: Expression t
    } deriving (Eq, Ord, Show)

data SyscallSSAOp t = SyscallSSAOp
    { _output :: Expression t
    , _params :: [Expression t]
    , _src_memory :: Int64
    } deriving (Eq, Ord, Show)

data SyscallUntypedSSAOp t = SyscallUntypedSSAOp
    { _output :: Expression t
    , _params :: Expression t
    , _stack :: Expression t
    } deriving (Eq, Ord, Show)

data TailcallSSAOp t = TailcallSSAOp
    { _output :: Expression t
    , _dest :: Expression t
    , _params :: [Expression t]
    , _src_memory :: Int64
    } deriving (Eq, Ord, Show)

data TailcallUntypedSSAOp t = TailcallUntypedSSAOp
    { _output :: Expression t
    , _dest :: Expression t
    , _params :: Expression t
    , _stack :: Expression t
    } deriving (Eq, Ord, Show)

data CallOutputSSAOp t = CallOutputSSAOp
    { _dest_memory :: Int64
    , _dest :: [SSAVariable]
    } deriving (Eq, Ord, Show)

data CallParamSSAOp t = CallParamSSAOp
    { _src_memory :: Int64
    , _src :: [SSAVariable]
    } deriving (Eq, Ord, Show)

data LoadSSAOp t = LoadSSAOp
    { _src :: Expression t
    , _src_memory :: Int64
    } deriving (Eq, Ord, Show)

data LoadStructSSAOp t = LoadStructSSAOp
    { _src :: Expression t
    , _offset :: Int64
    , _src_memory :: Int64
    } deriving (Eq, Ord, Show)

data StoreSSAOp t = StoreSSAOp
    { _dest :: Expression t
    , _dest_memory :: Int64
    , _src_memory :: Int64
    , _src :: Expression t
    } deriving (Eq, Ord, Show)

data StoreStructSSAOp t = StoreStructSSAOp
    { _dest :: Expression t
    , _offset :: Int64
    , _dest_memory :: Int64
    , _src_memory :: Int64
    , _src :: Expression t
    } deriving (Eq, Ord, Show)

data VarPhiOp t = VarPhiOp
    { _dest :: SSAVariable
    , _src :: [SSAVariable]
    } deriving (Eq, Ord, Show)

data MemPhiOp t = MemPhiOp
    { _dest_memory :: Int64
    , _src_memory :: [Int64]
    } deriving (Eq, Ord, Show)


$(makeFieldsNoPrefix ''NopOp)
$(makeFieldsNoPrefix ''SetVarOp)
$(makeFieldsNoPrefix ''SetVarFieldOp)
$(makeFieldsNoPrefix ''SetVarSplitOp)
$(makeFieldsNoPrefix ''LoadOp)
$(makeFieldsNoPrefix ''LoadStructOp)
$(makeFieldsNoPrefix ''StoreOp)
$(makeFieldsNoPrefix ''StoreStructOp)
$(makeFieldsNoPrefix ''VarOp)
$(makeFieldsNoPrefix ''VarFieldOp)
$(makeFieldsNoPrefix ''VarSplitOp)
$(makeFieldsNoPrefix ''AddressOfOp)
$(makeFieldsNoPrefix ''AddressOfFieldOp)
$(makeFieldsNoPrefix ''ConstOp)
$(makeFieldsNoPrefix ''ConstPtrOp)
$(makeFieldsNoPrefix ''ExternPtrOp)
$(makeFieldsNoPrefix ''FloatConstOp)
$(makeFieldsNoPrefix ''ImportOp)
$(makeFieldsNoPrefix ''AddOp)
$(makeFieldsNoPrefix ''AdcOp)
$(makeFieldsNoPrefix ''SubOp)
$(makeFieldsNoPrefix ''SbbOp)
$(makeFieldsNoPrefix ''AndOp)
$(makeFieldsNoPrefix ''OrOp)
$(makeFieldsNoPrefix ''XorOp)
$(makeFieldsNoPrefix ''LslOp)
$(makeFieldsNoPrefix ''LsrOp)
$(makeFieldsNoPrefix ''AsrOp)
$(makeFieldsNoPrefix ''RolOp)
$(makeFieldsNoPrefix ''RlcOp)
$(makeFieldsNoPrefix ''RorOp)
$(makeFieldsNoPrefix ''RrcOp)
$(makeFieldsNoPrefix ''MulOp)
$(makeFieldsNoPrefix ''MuluDpOp)
$(makeFieldsNoPrefix ''MulsDpOp)
$(makeFieldsNoPrefix ''DivuOp)
$(makeFieldsNoPrefix ''DivuDpOp)
$(makeFieldsNoPrefix ''DivsOp)
$(makeFieldsNoPrefix ''DivsDpOp)
$(makeFieldsNoPrefix ''ModuOp)
$(makeFieldsNoPrefix ''ModuDpOp)
$(makeFieldsNoPrefix ''ModsOp)
$(makeFieldsNoPrefix ''ModsDpOp)
$(makeFieldsNoPrefix ''NegOp)
$(makeFieldsNoPrefix ''NotOp)
$(makeFieldsNoPrefix ''SxOp)
$(makeFieldsNoPrefix ''ZxOp)
$(makeFieldsNoPrefix ''LowPartOp)
$(makeFieldsNoPrefix ''JumpOp)
$(makeFieldsNoPrefix ''JumpToOp)
$(makeFieldsNoPrefix ''RetHintOp)
$(makeFieldsNoPrefix ''CallOp)
$(makeFieldsNoPrefix ''CallUntypedOp)
$(makeFieldsNoPrefix ''CallOutputOp)
$(makeFieldsNoPrefix ''CallParamOp)
$(makeFieldsNoPrefix ''RetOp)
$(makeFieldsNoPrefix ''NoretOp)
$(makeFieldsNoPrefix ''IfOp)
$(makeFieldsNoPrefix ''GotoOp)
$(makeFieldsNoPrefix ''CmpEOp)
$(makeFieldsNoPrefix ''CmpNeOp)
$(makeFieldsNoPrefix ''CmpSltOp)
$(makeFieldsNoPrefix ''CmpUltOp)
$(makeFieldsNoPrefix ''CmpSleOp)
$(makeFieldsNoPrefix ''CmpUleOp)
$(makeFieldsNoPrefix ''CmpSgeOp)
$(makeFieldsNoPrefix ''CmpUgeOp)
$(makeFieldsNoPrefix ''CmpSgtOp)
$(makeFieldsNoPrefix ''CmpUgtOp)
$(makeFieldsNoPrefix ''TestBitOp)
$(makeFieldsNoPrefix ''BoolToIntOp)
$(makeFieldsNoPrefix ''AddOverflowOp)
$(makeFieldsNoPrefix ''SyscallOp)
$(makeFieldsNoPrefix ''SyscallUntypedOp)
$(makeFieldsNoPrefix ''TailcallOp)
$(makeFieldsNoPrefix ''TailcallUntypedOp)
$(makeFieldsNoPrefix ''BpOp)
$(makeFieldsNoPrefix ''TrapOp)
$(makeFieldsNoPrefix ''IntrinsicOp)
$(makeFieldsNoPrefix ''IntrinsicSSAOp)
$(makeFieldsNoPrefix ''FreeVarSlotOp)
$(makeFieldsNoPrefix ''FreeVarSlotSSAOp)
$(makeFieldsNoPrefix ''UndefOp)
$(makeFieldsNoPrefix ''UnimplOp)
$(makeFieldsNoPrefix ''UnimplMemOp)
$(makeFieldsNoPrefix ''FaddOp)
$(makeFieldsNoPrefix ''FsubOp)
$(makeFieldsNoPrefix ''FmulOp)
$(makeFieldsNoPrefix ''FdivOp)
$(makeFieldsNoPrefix ''FsqrtOp)
$(makeFieldsNoPrefix ''FnegOp)
$(makeFieldsNoPrefix ''FabsOp)
$(makeFieldsNoPrefix ''FloatToIntOp)
$(makeFieldsNoPrefix ''IntToFloatOp)
$(makeFieldsNoPrefix ''FloatConvOp)
$(makeFieldsNoPrefix ''RoundToIntOp)
$(makeFieldsNoPrefix ''FloorOp)
$(makeFieldsNoPrefix ''CeilOp)
$(makeFieldsNoPrefix ''FtruncOp)
$(makeFieldsNoPrefix ''FcmpEOp)
$(makeFieldsNoPrefix ''FcmpNeOp)
$(makeFieldsNoPrefix ''FcmpLtOp)
$(makeFieldsNoPrefix ''FcmpLeOp)
$(makeFieldsNoPrefix ''FcmpGeOp)
$(makeFieldsNoPrefix ''FcmpGtOp)
$(makeFieldsNoPrefix ''FcmpOOp)
$(makeFieldsNoPrefix ''FcmpUoOp)
$(makeFieldsNoPrefix ''SetVarSSAOp)
$(makeFieldsNoPrefix ''SetVarSSAFieldOp)
$(makeFieldsNoPrefix ''SetVarSplitSSAOp)
$(makeFieldsNoPrefix ''SetVarAliasedOp)
$(makeFieldsNoPrefix ''SetVarAliasedFieldOp)
$(makeFieldsNoPrefix ''VarSSAOp)
$(makeFieldsNoPrefix ''VarSSAFieldOp)
$(makeFieldsNoPrefix ''VarAliasedOp)
$(makeFieldsNoPrefix ''VarAliasedFieldOp)
$(makeFieldsNoPrefix ''VarSplitSSAOp)
$(makeFieldsNoPrefix ''CallSSAOp)
$(makeFieldsNoPrefix ''CallUntypedSSAOp)
$(makeFieldsNoPrefix ''SyscallSSAOp)
$(makeFieldsNoPrefix ''SyscallUntypedSSAOp)
$(makeFieldsNoPrefix ''TailcallSSAOp)
$(makeFieldsNoPrefix ''TailcallUntypedSSAOp)
$(makeFieldsNoPrefix ''CallOutputSSAOp)
$(makeFieldsNoPrefix ''CallParamSSAOp)
$(makeFieldsNoPrefix ''LoadSSAOp)
$(makeFieldsNoPrefix ''LoadStructSSAOp)
$(makeFieldsNoPrefix ''StoreSSAOp)
$(makeFieldsNoPrefix ''StoreStructSSAOp)
$(makeFieldsNoPrefix ''VarPhiOp)
$(makeFieldsNoPrefix ''MemPhiOp)


$(makeFieldsNoPrefix ''MediumLevelILInstruction)
$(makeFieldsNoPrefix ''OpBuilderCtx)
$(makeFieldsNoPrefix ''Expression)
$(makeFieldsNoPrefix ''Instruction)
$(makeFieldsNoPrefix ''SSAVariable)
$(makeFieldsNoPrefix ''SSAVariableDestAndSrc)

