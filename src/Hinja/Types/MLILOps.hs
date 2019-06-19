{-# LANGUAGE TemplateHaskell #-}

module Hinja.Types.MLILOps where

import Hinja.Prelude

import qualified Prelude as P
import Hinja.Types.Variable (Variable)
import Hinja.C.Types
import Hinja.Types.MLIL

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

data SSAVariable = SSAVariable { _var :: Variable
                               , _version :: Int
                               } deriving (Eq, Ord, Show)

data SSAVariableDestAndSrc = SSAVariableDestAndSrc
  { _dest :: SSAVariable
  , _src :: SSAVariable
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

data NopOp expr = NopOp
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data SetVarOp expr = SetVarOp
    { _dest :: Variable
    , _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data SetVarFieldOp expr = SetVarFieldOp
    { _dest :: Variable
    , _offset :: Int64
    , _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data SetVarSplitOp expr = SetVarSplitOp
    { _high :: Variable
    , _low :: Variable
    , _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data LoadOp expr = LoadOp
    { _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data LoadStructOp expr = LoadStructOp
    { _src :: expr
    , _offset :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data StoreOp expr = StoreOp
    { _dest :: expr
    , _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data StoreStructOp expr = StoreStructOp
    { _dest :: expr
    , _offset :: Int64
    , _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data VarOp expr = VarOp
    { _src :: Variable
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data VarFieldOp expr = VarFieldOp
    { _src :: Variable
    , _offset :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data VarSplitOp expr = VarSplitOp
    { _high :: Variable
    , _low :: Variable
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data AddressOfOp expr = AddressOfOp
    { _src :: Variable
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data AddressOfFieldOp expr = AddressOfFieldOp
    { _src :: Variable
    , _offset :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data ConstOp expr = ConstOp
    { _constant :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data ConstPtrOp expr = ConstPtrOp
    { _constant :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data ExternPtrOp expr = ExternPtrOp
    { _constant :: Int64
    , _offset :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data FloatConstOp expr = FloatConstOp
    { _constant :: Double
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data ImportOp expr = ImportOp
    { _constant :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data AddOp expr = AddOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data AdcOp expr = AdcOp
    { _left :: expr
    , _right :: expr
    , _carry :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data SubOp expr = SubOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data SbbOp expr = SbbOp
    { _left :: expr
    , _right :: expr
    , _carry :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data AndOp expr = AndOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data OrOp expr = OrOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data XorOp expr = XorOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data LslOp expr = LslOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data LsrOp expr = LsrOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data AsrOp expr = AsrOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data RolOp expr = RolOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data RlcOp expr = RlcOp
    { _left :: expr
    , _right :: expr
    , _carry :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data RorOp expr = RorOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data RrcOp expr = RrcOp
    { _left :: expr
    , _right :: expr
    , _carry :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data MulOp expr = MulOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data MuluDpOp expr = MuluDpOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data MulsDpOp expr = MulsDpOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data DivuOp expr = DivuOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data DivuDpOp expr = DivuDpOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data DivsOp expr = DivsOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data DivsDpOp expr = DivsDpOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data ModuOp expr = ModuOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data ModuDpOp expr = ModuDpOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data ModsOp expr = ModsOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data ModsDpOp expr = ModsDpOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data NegOp expr = NegOp
    { _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data NotOp expr = NotOp
    { _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data SxOp expr = SxOp
    { _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data ZxOp expr = ZxOp
    { _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data LowPartOp expr = LowPartOp
    { _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data JumpOp expr = JumpOp
    { _dest :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data JumpToOp expr = JumpToOp
    { _dest :: expr
    , _targets :: [Int64]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data RetHintOp expr = RetHintOp
    { _dest :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data CallOp expr = CallOp
    { _output :: [Variable]
    , _dest :: expr
    , _params :: [expr]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data CallUntypedOp expr = CallUntypedOp
    { _output :: expr
    , _dest :: expr
    , _params :: expr
    , _stack :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data CallOutputOp expr = CallOutputOp
    { _dest :: [Variable]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data CallParamOp expr = CallParamOp
    { _src :: [Variable]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data RetOp expr = RetOp
    { _src :: [expr]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data NoretOp expr = NoretOp
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data IfOp expr = IfOp
    { _condition :: expr
    , _true :: Int64
    , _false :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data GotoOp expr = GotoOp
    { _dest :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data CmpEOp expr = CmpEOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data CmpNeOp expr = CmpNeOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data CmpSltOp expr = CmpSltOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data CmpUltOp expr = CmpUltOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data CmpSleOp expr = CmpSleOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data CmpUleOp expr = CmpUleOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data CmpSgeOp expr = CmpSgeOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data CmpUgeOp expr = CmpUgeOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data CmpSgtOp expr = CmpSgtOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data CmpUgtOp expr = CmpUgtOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data TestBitOp expr = TestBitOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data BoolToIntOp expr = BoolToIntOp
    { _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data AddOverflowOp expr = AddOverflowOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data SyscallOp expr = SyscallOp
    { _output :: [Variable]
    , _params :: [expr]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data SyscallUntypedOp expr = SyscallUntypedOp
    { _output :: expr
    , _params :: expr
    , _stack :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data TailcallOp expr = TailcallOp
    { _output :: [Variable]
    , _dest :: expr
    , _params :: [expr]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data TailcallUntypedOp expr = TailcallUntypedOp
    { _output :: expr
    , _dest :: expr
    , _params :: expr
    , _stack :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data BpOp expr = BpOp
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data TrapOp expr = TrapOp
    { _vector :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data IntrinsicOp expr = IntrinsicOp
    { _output :: [Variable]
    , _intrinsic :: Intrinsic
    , _params :: [expr]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data IntrinsicSSAOp expr = IntrinsicSSAOp
    { _output :: [SSAVariable]
    , _intrinsic :: Intrinsic
    , _params :: [expr]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data FreeVarSlotOp expr = FreeVarSlotOp
    { _dest :: Variable
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data FreeVarSlotSSAOp expr = FreeVarSlotSSAOp
    { _prev :: SSAVariableDestAndSrc
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data UndefOp expr = UndefOp
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data UnimplOp expr = UnimplOp
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data UnimplMemOp expr = UnimplMemOp
    { _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data FaddOp expr = FaddOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data FsubOp expr = FsubOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data FmulOp expr = FmulOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data FdivOp expr = FdivOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data FsqrtOp expr = FsqrtOp
    { _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data FnegOp expr = FnegOp
    { _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data FabsOp expr = FabsOp
    { _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data FloatToIntOp expr = FloatToIntOp
    { _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data IntToFloatOp expr = IntToFloatOp
    { _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data FloatConvOp expr = FloatConvOp
    { _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data RoundToIntOp expr = RoundToIntOp
    { _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data FloorOp expr = FloorOp
    { _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data CeilOp expr = CeilOp
    { _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data FtruncOp expr = FtruncOp
    { _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data FcmpEOp expr = FcmpEOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data FcmpNeOp expr = FcmpNeOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data FcmpLtOp expr = FcmpLtOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data FcmpLeOp expr = FcmpLeOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data FcmpGeOp expr = FcmpGeOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data FcmpGtOp expr = FcmpGtOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data FcmpOOp expr = FcmpOOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data FcmpUoOp expr = FcmpUoOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data SetVarSSAOp expr = SetVarSSAOp
    { _dest :: SSAVariable
    , _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data SetVarSSAFieldOp expr = SetVarSSAFieldOp
    { _prev :: SSAVariableDestAndSrc
    , _offset :: Int64
    , _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data SetVarSplitSSAOp expr = SetVarSplitSSAOp
    { _high :: SSAVariable
    , _low :: SSAVariable
    , _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data SetVarAliasedOp expr = SetVarAliasedOp
    { _prev :: SSAVariableDestAndSrc
    , _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data SetVarAliasedFieldOp expr = SetVarAliasedFieldOp
    { _prev :: SSAVariableDestAndSrc
    , _offset :: Int64
    , _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data VarSSAOp expr = VarSSAOp
    { _src :: SSAVariable
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data VarSSAFieldOp expr = VarSSAFieldOp
    { _src :: SSAVariable
    , _offset :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data VarAliasedOp expr = VarAliasedOp
    { _src :: SSAVariable
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data VarAliasedFieldOp expr = VarAliasedFieldOp
    { _src :: SSAVariable
    , _offset :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data VarSplitSSAOp expr = VarSplitSSAOp
    { _high :: SSAVariable
    , _low :: SSAVariable
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data CallSSAOp expr = CallSSAOp
    { _output :: expr
    , _dest :: expr
    , _params :: [expr]
    , _src_memory :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data CallUntypedSSAOp expr = CallUntypedSSAOp
    { _output :: expr
    , _dest :: expr
    , _params :: expr
    , _stack :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data SyscallSSAOp expr = SyscallSSAOp
    { _output :: expr
    , _params :: [expr]
    , _src_memory :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data SyscallUntypedSSAOp expr = SyscallUntypedSSAOp
    { _output :: expr
    , _params :: expr
    , _stack :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data TailcallSSAOp expr = TailcallSSAOp
    { _output :: expr
    , _dest :: expr
    , _params :: [expr]
    , _src_memory :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data TailcallUntypedSSAOp expr = TailcallUntypedSSAOp
    { _output :: expr
    , _dest :: expr
    , _params :: expr
    , _stack :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data CallOutputSSAOp expr = CallOutputSSAOp
    { _dest_memory :: Int64
    , _dest :: [SSAVariable]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data CallParamSSAOp expr = CallParamSSAOp
    { _src_memory :: Int64
    , _src :: [SSAVariable]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data LoadSSAOp expr = LoadSSAOp
    { _src :: expr
    , _src_memory :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data LoadStructSSAOp expr = LoadStructSSAOp
    { _src :: expr
    , _offset :: Int64
    , _src_memory :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data StoreSSAOp expr = StoreSSAOp
    { _dest :: expr
    , _dest_memory :: Int64
    , _src_memory :: Int64
    , _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data StoreStructSSAOp expr = StoreStructSSAOp
    { _dest :: expr
    , _offset :: Int64
    , _dest_memory :: Int64
    , _src_memory :: Int64
    , _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data VarPhiOp expr = VarPhiOp
    { _dest :: SSAVariable
    , _src :: [SSAVariable]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data MemPhiOp expr = MemPhiOp
    { _dest_memory :: Int64
    , _src_memory :: [Int64]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)




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


$(makeFieldsNoPrefix ''OpBuilderCtx)
$(makeFieldsNoPrefix ''Expression)
$(makeFieldsNoPrefix ''Instruction)
$(makeFieldsNoPrefix ''SSAVariable)
$(makeFieldsNoPrefix ''SSAVariableDestAndSrc)

