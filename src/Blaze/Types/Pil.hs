{-# LANGUAGE TemplateHaskell #-}

module Blaze.Types.Pil
  ( module Exports,
    module Blaze.Types.Pil,
  )
where

import Binja.MLIL as Exports
  ( AdcOp (AdcOp),
    AddOp (AddOp),
    AddOverflowOp (AddOverflowOp),
    AndOp (AndOp),
    AsrOp (AsrOp),
    BoolToIntOp (BoolToIntOp),
    BpOp (BpOp),
    CallOutputOp (CallOutputOp),
    CallOutputSSAOp (CallOutputSSAOp),
    CallParamOp (CallParamOp),
    CallParamSSAOp (CallParamSSAOp),
    CallSSAOp (CallSSAOp),
    CallUntypedOp (CallUntypedOp),
    CallUntypedSSAOp (CallUntypedSSAOp),
    CeilOp (CeilOp),
    CmpEOp (CmpEOp),
    CmpNeOp (CmpNeOp),
    CmpSgeOp (CmpSgeOp),
    CmpSgtOp (CmpSgtOp),
    CmpSleOp (CmpSleOp),
    CmpSltOp (CmpSltOp),
    CmpUgeOp (CmpUgeOp),
    CmpUgtOp (CmpUgtOp),
    CmpUleOp (CmpUleOp),
    CmpUltOp (CmpUltOp),
    ConstOp (ConstOp),
    ConstPtrOp (ConstPtrOp),
    DivsDpOp (DivsDpOp),
    DivsOp (DivsOp),
    DivuDpOp (DivuDpOp),
    DivuOp (DivuOp),
    ExternPtrOp (ExternPtrOp),
    FabsOp (FabsOp),
    FaddOp (FaddOp),
    FcmpEOp (FcmpEOp),
    FcmpGeOp (FcmpGeOp),
    FcmpGtOp (FcmpGtOp),
    FcmpLeOp (FcmpLeOp),
    FcmpLtOp (FcmpLtOp),
    FcmpNeOp (FcmpNeOp),
    FcmpOOp (FcmpOOp),
    FcmpUoOp (FcmpUoOp),
    FdivOp (FdivOp),
    FloatConstOp (FloatConstOp),
    FloatConvOp (FloatConvOp),
    FloatToIntOp (FloatToIntOp),
    FloorOp (FloorOp),
    FmulOp (FmulOp),
    FnegOp (FnegOp),
    FreeVarSlotOp (FreeVarSlotOp),
    FreeVarSlotSSAOp (FreeVarSlotSSAOp),
    FsqrtOp (FsqrtOp),
    FsubOp (FsubOp),
    FtruncOp (FtruncOp),
    GotoOp (GotoOp),
    --TODO: do this for every xOp...

    HasCarry,
    HasCondition,
    HasConstant,
    HasDest,
    HasFunc,
    HasHigh,
    HasLeft,
    HasLow,
    HasOffset,
    HasOp,
    HasParams,
    HasRight,
    HasSize,
    HasSrc,
    HasVar,
    IfOp (IfOp),
    ImportOp (ImportOp),
    IntToFloatOp (IntToFloatOp),
    IntrinsicOp (IntrinsicOp),
    IntrinsicSSAOp (IntrinsicSSAOp),
    JumpOp (JumpOp),
    JumpToOp (JumpToOp),
    LoadOp (LoadOp),
    LoadSSAOp (LoadSSAOp),
    LoadStructOp (LoadStructOp),
    LoadStructSSAOp (LoadStructSSAOp),
    LowPartOp (LowPartOp),
    LslOp (LslOp),
    LsrOp (LsrOp),
    MemPhiOp (MemPhiOp),
    ModsDpOp (ModsDpOp),
    ModsOp (ModsOp),
    ModuDpOp (ModuDpOp),
    ModuOp (ModuOp),
    MulOp (MulOp),
    MulsDpOp (MulsDpOp),
    MuluDpOp (MuluDpOp),
    NegOp (NegOp),
    NoretOp (NoretOp),
    NotOp (NotOp),
    OperationSize (OperationSize),
    OrOp (OrOp),
    RetHintOp (RetHintOp),
    RetOp (RetOp),
    RlcOp (RlcOp),
    RolOp (RolOp),
    RorOp (RorOp),
    RoundToIntOp (RoundToIntOp),
    RrcOp (RrcOp),
    SSAVariable,
    SbbOp (SbbOp),
    SetVarAliasedFieldOp (SetVarAliasedFieldOp),
    SetVarAliasedOp (SetVarAliasedOp),
    SetVarFieldOp (SetVarFieldOp),
    SetVarOp (SetVarOp),
    SetVarSSAFieldOp (SetVarSSAFieldOp),
    SetVarSSAOp (SetVarSSAOp),
    SetVarSplitOp (SetVarSplitOp),
    SetVarSplitSSAOp (SetVarSplitSSAOp),
    StoreStructOp (StoreStructOp),
    StoreStructSSAOp (StoreStructSSAOp),
    SubOp (SubOp),
    SxOp (SxOp),
    SyscallOp (SyscallOp),
    SyscallSSAOp (SyscallSSAOp),
    SyscallUntypedOp (SyscallUntypedOp),
    SyscallUntypedSSAOp (SyscallUntypedSSAOp),
    TailcallOp (TailcallOp),
    TailcallSSAOp (TailcallSSAOp),
    TailcallUntypedOp (TailcallUntypedOp),
    TailcallUntypedSSAOp (TailcallUntypedSSAOp),
    TestBitOp (TestBitOp),
    TrapOp (TrapOp),
    UndefOp (UndefOp),
    UnimplOp (UnimplOp),
    XorOp (XorOp),
    ZxOp (ZxOp),
    address,
    carry,
    condition,
    constant,
    dest,
    dest_memory,
    exprIndex,
    false,
    func,
    high,
    index,
    intrinsic,
    left,
    low,
    offset,
    op,
    opData,
    operands,
    operation,
    output,
    params,
    prev,
    right,
    size,
    sourceOperand,
    src,
    src_memory,
    stack,
    targets,
    true,
    var,
    vector,
    version,
  )

import Binja.Function (Function)

import Blaze.Prelude hiding (Symbol, Type)
import Blaze.Types.Path.AlgaPath (AlgaPath)
import qualified Data.HashMap.Strict as HashMap

newtype CtxIndex = CtxIndex Int
  deriving (Eq, Ord, Show, Num, Generic)

instance Hashable CtxIndex

type Symbol = Text

-- Maybe is used to wrap _func and _ctxIndex since
-- contextual information may not be available or desirable
-- when introducing "synthetic" variables. (I.e., variables
-- which do not correspond to variables in the source program.)
data PilVar = PilVar
  { _symbol :: Symbol
  -- TODO: Reassess use of Maybe for ctx. 
  --       Currently needed when introducing synthesized PilVars
  --       when replacing store statements. May also be useful for 
  --       introducing arbitrary symbols used in constraints?
  --       Another option is to explicitly use a default context
  --       Related to this is having Blaze.Pil.Construct functions
  --       play nice with context management.
  , _ctx :: Maybe Ctx
  , _mapsTo :: HashSet SSAVariableRef
  } deriving (Eq, Ord, Show, Generic)
           
instance Hashable PilVar

data ConverterState
  = ConverterState
      { -- The path being converted
        _path :: AlgaPath,
        -- The maximum context ID used so far
        _ctxMaxIdx :: CtxIndex,
        -- The current context should be on the top of the stack.
        -- I.e., the stack should never be empty.
        _ctxStack :: NonEmpty Ctx,
        -- The current context
        _ctx :: Ctx,
        -- Currently known defined PilVars for all contexts.
        -- This is assumed to be ordered by most recently defined first.
        -- TODO: Can we safeguard for overwriting/colliding with already used PilVars?
        --       This could happen for synthesized PilVars with a Nothing context.
        _definedVars :: [PilVar]
      }
  deriving (Eq, Ord, Show, Generic)

-- TODO: Add map of PilVars to original vars to the state being tracked
newtype Converter a = Converter { _runConverter :: StateT ConverterState IO a}
  deriving (Functor, Applicative, Monad, MonadState ConverterState, MonadIO)

runConverter :: Converter a -> ConverterState -> IO (a, ConverterState)
runConverter m s = flip runStateT s $ _runConverter m

data SSAVariableRef = SSAVariableRef
  { _var :: SSAVariable
  , _func :: Function
  , _ctxIndex :: CtxIndex
  } deriving (Eq, Ord, Show, Generic)

instance Hashable SSAVariableRef

data Expression = Expression
  { _size :: OperationSize
  , _op :: ExprOp Expression
  } deriving (Eq, Ord, Show, Generic)

instance Hashable Expression

data ExprOp expr
    = ADC (AdcOp expr) --
    | ADD (AddOp expr)
    | ADD_OVERFLOW (AddOverflowOp expr) --
    | AND (AndOp expr)
    | ASR (AsrOp expr) --
    | BOOL_TO_INT (BoolToIntOp expr)
    | CEIL (CeilOp expr) -- 
    | CMP_E (CmpEOp expr)
    | CMP_NE (CmpNeOp expr)
    | CMP_SGE (CmpSgeOp expr)
    | CMP_SGT (CmpSgtOp expr)
    | CMP_SLE (CmpSleOp expr)
    | CMP_SLT (CmpSltOp expr)
    | CMP_UGE (CmpUgeOp expr)
    | CMP_UGT (CmpUgtOp expr)
    | CMP_ULE (CmpUleOp expr)
    | CMP_ULT (CmpUltOp expr)
    | CONST (ConstOp expr)
    | CONST_PTR (ConstPtrOp expr)
    | DIVS (DivsOp expr)
    | DIVS_DP (DivsDpOp expr)
    | DIVU (DivuOp expr)
    | DIVU_DP (DivuDpOp expr)
    | FABS (FabsOp expr)
    | FADD (FaddOp expr) -- 
    | FCMP_E (FcmpEOp expr)
    | FCMP_GE (FcmpGeOp expr)
    | FCMP_GT (FcmpGtOp expr)
    | FCMP_LE (FcmpLeOp expr)
    | FCMP_LT (FcmpLtOp expr)
    | FCMP_NE (FcmpNeOp expr)
    | FCMP_O (FcmpOOp expr)
    | FCMP_UO (FcmpUoOp expr)
    | FDIV (FdivOp expr) -- 
    | FLOAT_CONST (FloatConstOp expr)
    | FLOAT_CONV (FloatConvOp expr)
    | FLOAT_TO_INT (FloatToIntOp expr) --
    | FLOOR (FloorOp expr) -- 
    | FMUL (FmulOp expr)
    | FNEG (FnegOp expr)
    | FSQRT (FsqrtOp expr)
    | FSUB (FsubOp expr)
    | FTRUNC (FtruncOp expr)
    | IMPORT (ImportOp expr)
    | INT_TO_FLOAT (IntToFloatOp expr)
    | LOAD (LoadOp expr)
    | LOW_PART (LowPartOp expr) -- 
    | LSL (LslOp expr)
    | LSR (LsrOp expr)
    | MODS (ModsOp expr)
    | MODS_DP (ModsDpOp expr)
    | MODU (ModuOp expr)
    | MODU_DP (ModuDpOp expr)
    | MUL (MulOp expr)
    | MULS_DP (MulsDpOp expr)
    | MULU_DP (MuluDpOp expr)
    | NEG (NegOp expr)
    | NOT (NotOp expr)
    | OR (OrOp expr)
    | RLC (RlcOp expr)
    | ROL (RolOp expr)
    | ROR (RorOp expr)
    | ROUND_TO_INT (RoundToIntOp expr)
    | RRC (RrcOp expr)
    | SBB (SbbOp expr)
    | SUB (SubOp expr)
    | SX (SxOp expr)
    | TEST_BIT (TestBitOp expr)
    | UNIMPL Text
    | VAR_PHI (VarPhiOp expr)
    | VAR_SPLIT (VarSplitOp expr)
    | VAR (VarOp expr)
    | VAR_FIELD (VarFieldOp expr)
    | XOR (XorOp expr)
    | ZX (ZxOp expr)

    | CALL (CallOp expr)

    | Extract (ExtractOp expr) -- 
    | StrCmp (StrCmpOp expr) 
    | StrNCmp (StrNCmpOp expr) -- 
    | MemCmp (MemCmpOp expr)
    | ConstStr (ConstStrOp expr)
    | STACK_LOCAL_ADDR (StackLocalAddrOp expr)
    | FIELD_ADDR (FieldAddrOp expr)
    | UPDATE_VAR (UpdateVarOp expr)
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (ExprOp a)

-------- Ops that use MLIL SSA Vars must be changed to use PilVars


{- HLINT ignore VarOp -}
data VarOp expr = VarOp
    { _varOpSrc :: PilVar
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (VarOp a)

data VarFieldOp expr = VarFieldOp
    { _varFieldOpSrc :: PilVar
    , _varFieldOpOffset :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (VarFieldOp a)

data VarPhiOp expr = VarPhiOp
    { _varPhiOpDest :: PilVar
    , _varPhiOpSrc :: [PilVar]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (VarPhiOp a)

data VarSplitOp expr = VarSplitOp
    { _varSplitOpHigh :: PilVar
    , _varSplitOpLow :: PilVar
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (VarSplitOp a)

--TODO: address_of and address_of_field
---------------

getCallDest :: Expression -> CallDest Expression
getCallDest expr = case expr ^. op of
  (CONST_PTR c) -> CallConstPtr c
  _ -> CallExpr expr

data CallDest expr = CallConstPtr (ConstPtrOp expr)
                   | CallExpr expr
                   | CallExprs [expr]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (CallDest a)

data CallOp expr = CallOp
  { _dest :: CallDest expr
  , _name :: Maybe Text
  , _params :: [expr]
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (CallOp a)

data ExtractOp expr = ExtractOp
    { _src :: expr
    , _offset :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (ExtractOp a)


data StrCmpOp expr = StrCmpOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (StrCmpOp a)

data StrNCmpOp expr = StrNCmpOp
    { _left :: expr
    , _right :: expr
    , _len :: Int
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (StrNCmpOp a)

data MemCmpOp expr = MemCmpOp
    { _left :: expr
    , _right :: expr
    , _size :: Bytes
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (MemCmpOp a)

{- HLINT ignore ConstStrOp -}
data ConstStrOp expr = ConstStrOp
    { _value :: Text
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (ConstStrOp a)

{- HLINT ignore StackLocalAddrOp -}
data StackLocalAddrOp expr = StackLocalAddrOp
    { _src :: StackOffset
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
instance Hashable a => Hashable (StackLocalAddrOp a)

{- HLINT ignore FieldAddrOp -}
data FieldAddrOp expr = FieldAddrOp
    { _baseAddr :: expr
    , _offset :: ByteOffset
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
instance Hashable a => Hashable (FieldAddrOp a)

{- HLINT ignore UpdateVarOp -}
data UpdateVarOp expr = UpdateVarOp
    { _dest :: PilVar
    , _offset :: ByteOffset
    , _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
instance Hashable a => Hashable (UpdateVarOp a)

-----------------------
--- types

data TypedExpression = TypedExpression
  { _exprType :: Type
  , _size :: Bytes
  , _op :: ExprOp Expression
  } deriving (Eq, Ord, Show, Generic)
instance Hashable TypedExpression

newtype BitVecType = BitVecType
  { _width :: Bytes
  } deriving (Eq, Ord, Show, Generic)
instance Hashable BitVecType

data IntType = IntType
  { _width :: Bytes
  , _signed :: Bool
  } deriving (Eq, Ord, Show, Generic)
instance Hashable IntType

{- HLINT ignore FloatType -}
data FloatType = FloatType
  { _width :: Bytes
  } deriving (Eq, Ord, Show, Generic)
instance Hashable FloatType

data ArrayType = ArrayType
  { _capacity :: Int
  , _elemType :: Type
  } deriving (Eq, Ord, Show, Generic)
instance Hashable ArrayType

data PtrType = PtrType
  { _width :: Bytes
  , _pointeeType :: Type
  } deriving (Eq, Ord, Show, Generic)
instance Hashable PtrType

data FieldType = FieldType
  { _offset :: Bytes
  , _fieldType :: Type
  } deriving (Eq, Ord, Show, Generic)

instance Hashable FieldType

data StructType = StructType
  { _size :: Bytes
  , _fields :: [Type]
  } deriving (Eq, Ord, Show, Generic)
instance Hashable StructType

data FuncType = FuncType
  { _args :: [Type]
  , _ret :: Maybe Type
  } deriving (Eq, Ord, Show, Generic)
instance Hashable FuncType

type ObsType = [Type]

data Type = TBool
          | TBitVec BitVecType
          | TInt IntType
          | TFloat FloatType
          | TArray ArrayType
          | TPtr PtrType
          | TField FieldType
          | TString
          | TStruct StructType
          | TObs ObsType
          | TFunc FuncType
          deriving (Eq, Ord, Show, Generic)
instance Hashable Type

instance Semigroup Type where
  (<>) (TObs obs1) (TObs obs2) = TObs $ obs1 <> obs2
  (<>) (TObs obs) t = TObs $ obs <> [t] --to keep associativity
  (<>) t (TObs obs) = TObs $ t:obs
  (<>) t1 t2
    | t1 == t2 = t1
    | otherwise = TObs [t1, t2]

instance Monoid Type where
  mempty = TObs []

newtype TypeEnv = TypeEnv (HashMap PilVar Type)
  deriving (Eq, Ord, Show, Generic)
instance Hashable TypeEnv

typeEnvLookup :: PilVar -> TypeEnv -> Maybe Type
typeEnvLookup v (TypeEnv env) = HashMap.lookup v env

instance Semigroup TypeEnv where
  (<>) (TypeEnv m1) (TypeEnv m2) = TypeEnv $ HashMap.unionWith (<>) m1 m2

instance Monoid TypeEnv where
  mempty = TypeEnv HashMap.empty

------

data Ctx = Ctx
  { _func :: Function
  , _ctxIndex :: CtxIndex
  } deriving (Eq, Ord, Show, Generic)
instance Hashable Ctx

data StackOffset = StackOffset
  { _ctx :: Ctx
  , _offset :: ByteOffset
  } deriving (Eq, Ord, Show, Generic)
instance Hashable StackOffset

type Keyword = Text

data Label = StackOffsetLabel StackOffset
           | KeywordLabel Keyword
           deriving (Eq, Ord, Show, Generic)

newtype Storage = Storage
  { _label :: Label
  } deriving (Eq, Ord, Show, Generic)

---- Statements

data DefOp expr = DefOp
    { _var :: PilVar
    , _value :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
instance Hashable a => Hashable (DefOp a)

data StoreOp expr = StoreOp
    { _addr :: expr
    , _value :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
instance Hashable a => Hashable (StoreOp a)

{- HLINT ignore ConstraintOp -}
data ConstraintOp expr = ConstraintOp
    { _condition :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
instance Hashable a => Hashable (ConstraintOp a)

{- HLINT ignore UnimplMemOp -}
data UnimplMemOp expr = UnimplMemOp
    { _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
instance Hashable a => Hashable (UnimplMemOp a)

{- HLINT ignore EnterContextOp -}
data EnterContextOp expr = EnterContextOp
    { _ctx :: Ctx
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
instance Hashable a => Hashable (EnterContextOp a)

data ExitContextOp expr = ExitContextOp
    { _leavingCtx :: Ctx
    , _returningToCtx :: Ctx
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
instance Hashable a => Hashable (ExitContextOp a)

type Stmt = Statement Expression

data Statement expr = Def (DefOp expr)
                    | Constraint (ConstraintOp expr)
                    | Store (StoreOp expr)
                    | UnimplInstr Text
                    | UnimplMem (UnimplMemOp expr)
                    | Undef
                    | Nop
                    | Annotation Text
                    | EnterContext (EnterContextOp expr)
                    | ExitContext (ExitContextOp expr)
                    | Call (CallOp expr)
                    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
instance Hashable a => Hashable (Statement a)

$(makeFields ''VarOp)
$(makeFields ''VarFieldOp)
$(makeFields ''VarPhiOp)
$(makeFields ''VarSplitOp)

$(makeFieldsNoPrefix ''StackLocalAddrOp)
$(makeFieldsNoPrefix ''FieldAddrOp)
$(makeFieldsNoPrefix ''UpdateVarOp)

$(makeFieldsNoPrefix ''SSAVariableRef)
$(makeFieldsNoPrefix ''PilVar)
$(makeFieldsNoPrefix ''Expression)
$(makeFieldsNoPrefix ''CallOp)
$(makeFieldsNoPrefix ''ExtractOp)
$(makeFieldsNoPrefix ''StrCmpOp)
$(makeFieldsNoPrefix ''StrNCmpOp)
$(makeFieldsNoPrefix ''MemCmpOp)
$(makeFieldsNoPrefix ''ConstStrOp)
$(makeFieldsNoPrefix ''EnterContextOp)
$(makeFieldsNoPrefix ''ExitContextOp)

$(makeFieldsNoPrefix ''TypedExpression)
$(makeFieldsNoPrefix ''BitVecType)
$(makeFieldsNoPrefix ''IntType)
$(makeFieldsNoPrefix ''FloatType)
$(makeFieldsNoPrefix ''ArrayType)
$(makeFieldsNoPrefix ''PtrType)
$(makeFieldsNoPrefix ''FieldType)
$(makeFieldsNoPrefix ''StructType)
$(makeFieldsNoPrefix ''Ctx)
$(makeFieldsNoPrefix ''ConverterState)
$(makeFieldsNoPrefix ''StackOffset)
$(makeFieldsNoPrefix ''Storage)

$(makeFieldsNoPrefix ''DefOp)
$(makeFieldsNoPrefix ''StoreOp)
$(makeFieldsNoPrefix ''UnimplMemOp)
$(makeFieldsNoPrefix ''ConstraintOp)

$(makePrisms ''ExprOp)
$(makePrisms ''Type)
$(makePrisms ''Statement)
------------------------

-- gets bit width of integral type, if available
getTypeByteWidth :: Type -> Maybe Bytes
getTypeByteWidth (TBitVec x) = Just $ x ^. width
getTypeByteWidth (TInt x) = Just $ x ^. width
getTypeByteWidth (TPtr x) = Just $ x ^. width
getTypeByteWidth (TFloat x) = Just $ x ^. width
getTypeByteWidth _ = Nothing

getTypeBitWidth :: Type -> Maybe Bits
getTypeBitWidth = fmap toBits . getTypeByteWidth

getSignedness :: Type -> Maybe Bool
getSignedness (TBitVec _) = Just False
getSignedness (TInt x) = Just $ x ^. signed
getSignedness (TPtr _) = Just False
getSignedness (TFloat _) = Just True --floats are always signed?
getSignedness _ = Nothing

