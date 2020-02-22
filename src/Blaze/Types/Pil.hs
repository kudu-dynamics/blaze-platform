{-# LANGUAGE TemplateHaskell #-}
module Blaze.Types.Pil
  ( module Exports
  , module Blaze.Types.Pil
  ) where

import Blaze.Prelude hiding (Symbol, Type)

import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)

import Binja.MLIL as Exports ( AdcOp(AdcOp)
                             , AddOp(AddOp)
                             , AddOverflowOp
                             , AddressOfFieldOp(AddressOfFieldOp)
                             , AddressOfOp(AddressOfOp)
                             , AndOp(AndOp)
                             , AsrOp(AsrOp)
                             , BoolToIntOp(BoolToIntOp)
                             , BpOp(BpOp)
                             , CallOutputOp(CallOutputOp)
                             , CallOutputSSAOp(CallOutputSSAOp)
                             , CallParamOp(CallParamOp)
                             , CallParamSSAOp(CallParamSSAOp)
                             , CallSSAOp(CallSSAOp)
                             , CallUntypedOp(CallUntypedOp)
                             , CallUntypedSSAOp(CallUntypedSSAOp)
                             , CeilOp(CeilOp)
                             , CmpEOp(CmpEOp)
                             , CmpNeOp(CmpNeOp)
                             , CmpSgeOp(CmpSgeOp)
                             , CmpSgtOp(CmpSgtOp)
                             , CmpSleOp(CmpSleOp)
                             , CmpSltOp(CmpSltOp)
                             , CmpUgeOp(CmpUgeOp)
                             , CmpUgtOp(CmpUgtOp)
                             , CmpUleOp(CmpUleOp)
                             , CmpUltOp(CmpUltOp)
                             , ConstOp(ConstOp)
                             , ConstPtrOp(ConstPtrOp)
                             , DivsDpOp(DivsDpOp)
                             , DivsOp(DivsOp)
                             , DivuDpOp(DivuDpOp)
                             , DivuOp(DivuOp)
                             , ExternPtrOp(ExternPtrOp)
                             , FabsOp(FabsOp)
                             , FaddOp(FaddOp)
                             , FcmpEOp(FcmpEOp)
                             , FcmpGeOp(FcmpGeOp)
                             , FcmpGtOp(FcmpGtOp)
                             , FcmpLeOp(FcmpLeOp)
                             , FcmpLtOp(FcmpLtOp)
                             , FcmpNeOp(FcmpNeOp)
                             , FcmpOOp(FcmpOOp)
                             , FcmpUoOp(FcmpUoOp)
                             , FdivOp(FdivOp)
                             , FloatConstOp(FloatConstOp)
                             , FloatConvOp(FloatConvOp)
                             , FloatToIntOp(FloatToIntOp)
                             , FloorOp(FloorOp)
                             , FmulOp(FmulOp)
                             , FnegOp(FnegOp)
                             , FreeVarSlotOp(FreeVarSlotOp)
                             , FreeVarSlotSSAOp(FreeVarSlotSSAOp)
                             , FsqrtOp(FsqrtOp)
                             , FsubOp(FsubOp)
                             , FtruncOp(FtruncOp)
                             , GotoOp(GotoOp)
                             , IfOp(IfOp)
                             , ImportOp(ImportOp)
                             , IntToFloatOp(IntToFloatOp)
                             , IntrinsicOp(IntrinsicOp)
                             , IntrinsicSSAOp(IntrinsicSSAOp)
                             , JumpOp(JumpOp)
                             , JumpToOp(JumpToOp)
                             , LoadOp(LoadOp)
                             , LoadSSAOp(LoadSSAOp)
                             , LoadStructOp(LoadStructOp)
                             , LoadStructSSAOp(LoadStructSSAOp)
                             , LowPartOp(LowPartOp)
                             , LslOp(LslOp)
                             , LsrOp(LsrOp)
                             , MemPhiOp(MemPhiOp)
                             , ModsDpOp(ModsDpOp)
                             , ModsOp(ModsOp)
                             , ModuDpOp(ModuDpOp)
                             , ModuOp(ModuOp)
                             , MulOp(MulOp)
                             , MulsDpOp(MulsDpOp)
                             , MuluDpOp(MuluDpOp)
                             , NegOp(NegOp)
                             , NoretOp(NoretOp)
                             , NotOp(NotOp) --TODO: do this for every xOp...
                             , OrOp(OrOp)
                             , RetHintOp(RetHintOp)
                             , RetOp(RetOp)
                             , RlcOp(RlcOp)
                             , RolOp(RolOp)
                             , RorOp(RorOp)
                             , RoundToIntOp(RoundToIntOp)
                             , RrcOp(RrcOp)
                             , SbbOp(SbbOp)
                             , SetVarAliasedFieldOp(SetVarAliasedFieldOp)
                             , SetVarAliasedOp(SetVarAliasedOp)
                             , SetVarFieldOp(SetVarFieldOp)
                             , SetVarOp(SetVarOp)
                             , SetVarSSAFieldOp(SetVarSSAFieldOp)
                             , SetVarSSAOp(SetVarSSAOp)
                             , SetVarSplitOp(SetVarSplitOp)
                             , SetVarSplitSSAOp(SetVarSplitSSAOp)
                             , StoreStructOp(StoreStructOp)
                             , StoreStructSSAOp(StoreStructSSAOp)
                             , SubOp(SubOp)
                             , SxOp(SxOp)
                             , SyscallOp(SyscallOp)
                             , SyscallSSAOp(SyscallSSAOp)
                             , SyscallUntypedOp(SyscallUntypedOp)
                             , SyscallUntypedSSAOp(SyscallUntypedSSAOp)
                             , TailcallOp(TailcallOp)
                             , TailcallSSAOp(TailcallSSAOp)
                             , TailcallUntypedOp(TailcallUntypedOp)
                             , TailcallUntypedSSAOp(TailcallUntypedSSAOp)
                             , TestBitOp(TestBitOp)
                             , TrapOp(TrapOp)
                             , UndefOp(UndefOp)
                             , UnimplOp(UnimplOp)
                             , XorOp(XorOp)
                             , ZxOp(ZxOp)
                             , OperationSize(OperationSize)
                             , SSAVariable
                             , HasSize
                             , HasOp
                             , HasCondition
                             , HasConstant
                             , HasDest
                             , HasFunc
                             , HasLeft
                             , HasOffset
                             , HasParams
                             , HasRight
                             , HasVar
                             , HasSrc
                             , HasHigh
                             , HasLow
                             , index
                             , func
                             , operation
                             , sourceOperand
                             , size
                             , operands
                             , address
                             , exprIndex
                             , opData
                             , op
                             , var
                             , version
                             , dest
                             , src
                             , offset
                             , high
                             , low
                             , constant
                             , left
                             , right
                             , carry
                             , targets
                             , params
                             , output
                             , stack
                             , condition
                             , true
                             , false
                             , vector
                             , intrinsic
                             , prev
                             , src_memory
                             , dest_memory
                             )

import Binja.Function (Function)

newtype CtxIndex = CtxIndex Int
  deriving (Eq, Ord, Show, Num, Generic)

instance Hashable CtxIndex

type Symbol = Text

data PilVar = PilVar
  { _symbol :: Symbol
  , _func :: Maybe Function
  , _ctxIndex :: Maybe CtxIndex
  , _mapsTo :: HashSet SSAVariableRef
  } deriving (Eq, Ord, Show, Generic)

-- instance Hashable PilVar where
--   hashWithSalt s (PilVar symbol mf mc ss) =
--     s `hashWithSalt` symbol
--       `hashWithSalt` mf
--       `hashWithSalt` mc
--       `hashWithSalt` ss

instance Hashable PilVar

data ConverterCtx = ConverterCtx
  { _ctxIndexCounter :: Maybe CtxIndex
  , _ctx :: Ctx
  } deriving (Eq, Ord, Show, Generic)

newtype Converter a = Converter { _runConverter :: StateT ConverterCtx IO a}
  deriving (Functor, Applicative, Monad, MonadState ConverterCtx, MonadIO)

runConverter :: Converter a -> ConverterCtx -> IO (a, ConverterCtx)
runConverter m s = flip runStateT s $ _runConverter m

data SSAVariableRef = SSAVariableRef
  { _var :: SSAVariable
  , _func :: Maybe Function
  , _ctxIndex :: Maybe CtxIndex
  } deriving (Eq, Ord, Show, Generic)

instance Hashable SSAVariableRef where
  hashWithSalt s (SSAVariableRef v mf mc) = 
    s `hashWithSalt`
    v `hashWithSalt`
    mf `hashWithSalt` mc

--- maybe should use higher kinded types for this
data MExpression = MExpression
  { _size :: OperationSize
  , _op :: Maybe (ExprOp MExpression)
  } deriving (Eq, Ord, Show, Generic)

data Expression = Expression
  { _size :: OperationSize
  , _op :: ExprOp Expression
  } deriving (Eq, Ord, Show, Generic)

instance Hashable Expression

data ExprOp expr
    = ADC (AdcOp expr)
    | ADD (AddOp expr)
    | ADDRESS_OF (AddressOfOp expr)
    | ADDRESS_OF_FIELD (AddressOfFieldOp expr)
    | ADD_OVERFLOW (AddOverflowOp expr)
    | AND (AndOp expr)
    | ASR (AsrOp expr)
    | BOOL_TO_INT (BoolToIntOp expr)
    | CEIL (CeilOp expr)
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
    | FADD (FaddOp expr)
    | FCMP_E (FcmpEOp expr)
    | FCMP_GE (FcmpGeOp expr)
    | FCMP_GT (FcmpGtOp expr)
    | FCMP_LE (FcmpLeOp expr)
    | FCMP_LT (FcmpLtOp expr)
    | FCMP_NE (FcmpNeOp expr)
    | FCMP_O (FcmpOOp expr)
    | FCMP_UO (FcmpUoOp expr)
    | FDIV (FdivOp expr)
    | FLOAT_CONST (FloatConstOp expr)
    | FLOAT_CONV (FloatConvOp expr)
    | FLOAT_TO_INT (FloatToIntOp expr)
    | FLOOR (FloorOp expr)
    | FMUL (FmulOp expr)
    | FNEG (FnegOp expr)
    | FSQRT (FsqrtOp expr)
    | FSUB (FsubOp expr)
    | FTRUNC (FtruncOp expr)
    | IMPORT (ImportOp expr)
    | INT_TO_FLOAT (IntToFloatOp expr)
    | LOAD (LoadOp expr)
    | LOAD_SSA (LoadSSAOp expr)
    | LOAD_STRUCT (LoadStructOp expr)
    | LOAD_STRUCT_SSA (LoadStructSSAOp expr)
    | LOW_PART (LowPartOp expr)
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
    | UNIMPL
    | VAR_ALIASED (VarAliasedOp expr)
    | VAR_ALIASED_FIELD (VarAliasedFieldOp expr)
    | VAR_PHI (VarPhiOp expr)
    | VAR_SPLIT (VarSplitOp expr)
    | VAR (VarOp expr)
    | VAR_FIELD (VarFieldOp expr)
    | XOR (XorOp expr)
    | ZX (ZxOp expr)

    | CALL (CallOp expr)

    | StrCmp (StrCmpOp expr)
    | StrNCmp (StrNCmpOp expr)
    | MemCmp (MemCmpOp expr)
    | ConstStr (ConstStrOp expr)
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

{- HLINT ignore VarAliasedOp -}
data VarAliasedOp expr = VarAliasedOp
    { _varAliasedOpSrc :: PilVar
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (VarAliasedOp a)

data VarAliasedFieldOp expr = VarAliasedFieldOp
    { _varAliasedFieldOpSrc :: PilVar
    , _varAliasedFieldOpOffset :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (VarAliasedFieldOp a)

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
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (MemCmpOp a)

{- HLINT ignore ConstStrOp -}
data ConstStrOp expr = ConstStrOp
    { _value :: Text
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (ConstStrOp a)

-----------------------
--- types

data TypedExpression = TypedExpression
  { _exprType :: Type
  , _size :: OperationSize
  , _op :: ExprOp Expression
  } deriving (Eq, Ord, Show, Generic)
instance Hashable TypedExpression

newtype BitVecType = BitVecType
  { _width :: Int
  } deriving (Eq, Ord, Show, Generic)
instance Hashable BitVecType

data IntType = IntType
  { _width :: Int
  , _signed :: Bool
  } deriving (Eq, Ord, Show, Generic)
instance Hashable IntType

{- HLINT ignore FloatType -}
data FloatType = FloatType
  { _width :: Int
  } deriving (Eq, Ord, Show, Generic)
instance Hashable FloatType

data ArrayType = ArrayType
  { _capacity :: Int
  , _elemType :: Type
  } deriving (Eq, Ord, Show, Generic)
instance Hashable ArrayType

data PtrType = PtrType
  { _width :: Int
  , _pointeeType :: Type
  } deriving (Eq, Ord, Show, Generic)
instance Hashable PtrType

data FieldType = FieldType
  { _offset :: Int
  , _fieldType :: Type
  } deriving (Eq, Ord, Show, Generic)
instance Hashable FieldType

data StructType = StructType
  { _size :: Int
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
           
type TypeEnv = HashMap PilVar Type

------

data Ctx = Ctx
  { _func :: Maybe Function
  , _ctxIndex :: Maybe CtxIndex
  , _definedVars :: HashSet PilVar
  , _typeEnv :: TypeEnv
  } deriving (Eq, Ord, Show, Generic)
instance Hashable Ctx

data SimpleCtx = SimpleCtx
  { _func :: Maybe Function
  , _ctxIndex :: Maybe CtxIndex
  } deriving (Eq, Ord, Show, Generic)
instance Hashable SimpleCtx

data StackOffset = StackOffset
  { _func :: Function
  , _ctxIndex :: CtxIndex
  , _offset :: Int
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
    { _ctx :: SimpleCtx
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
instance Hashable a => Hashable (EnterContextOp a)

data ExitContextOp expr = ExitContextOp
    { _leavingCtx :: SimpleCtx
    , _returningToCtx :: SimpleCtx
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
instance Hashable a => Hashable (ExitContextOp a)

type Stmt = Statement Expression

data Statement expr = Def (DefOp expr)
                    | Constraint (ConstraintOp expr)
                    | Store (StoreOp expr)
                    | UnimplInstr
                    | UnimplMem (UnimplMemOp expr)
                    | Undef
                    | Nop
                    | Annotation Text
                    | EnterContext (EnterContextOp expr)
                    | ExitContext (ExitContextOp expr)
                    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
instance Hashable a => Hashable (Statement a)

$(makeFields ''VarOp)
$(makeFields ''VarFieldOp)
$(makeFields ''VarAliasedOp)
$(makeFields ''VarAliasedFieldOp)
$(makeFields ''VarPhiOp)
$(makeFields ''VarSplitOp)

$(makeFieldsNoPrefix ''SSAVariableRef)
$(makeFieldsNoPrefix ''PilVar)
$(makeFieldsNoPrefix ''Expression)
$(makeFieldsNoPrefix ''MExpression)
$(makeFieldsNoPrefix ''CallOp)
$(makeFieldsNoPrefix ''StrCmpOp)
$(makeFieldsNoPrefix ''StrNCmpOp)
$(makeFieldsNoPrefix ''MemCmpOp)
$(makeFieldsNoPrefix ''ConstStrOp)
$(makeFieldsNoPrefix ''TypedExpression)
$(makeFieldsNoPrefix ''BitVecType)
$(makeFieldsNoPrefix ''IntType)
$(makeFieldsNoPrefix ''FloatType)
$(makeFieldsNoPrefix ''ArrayType)
$(makeFieldsNoPrefix ''PtrType)
$(makeFieldsNoPrefix ''FieldType)
$(makeFieldsNoPrefix ''StructType)
$(makeFieldsNoPrefix ''Ctx)
$(makeFieldsNoPrefix ''SimpleCtx)
$(makeFieldsNoPrefix ''ConverterCtx)
$(makeFieldsNoPrefix ''StackOffset)
$(makeFieldsNoPrefix ''Storage)

$(makeFieldsNoPrefix ''DefOp)
$(makeFieldsNoPrefix ''StoreOp)
$(makeFieldsNoPrefix ''UnimplMemOp)
$(makeFieldsNoPrefix ''ConstraintOp)
$(makeFieldsNoPrefix ''EnterContextOp)
$(makeFieldsNoPrefix ''ExitContextOp)
