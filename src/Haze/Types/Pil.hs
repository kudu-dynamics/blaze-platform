{-# LANGUAGE TemplateHaskell #-}
module Haze.Types.Pil
  ( module Exports
  , module Haze.Types.Pil
  ) where

import Haze.Prelude hiding (Symbol, Type)

import Hinja.MLIL as Exports ( AdcOp
                             , AddOp
                             , AddOverflowOp
                             , AddressOfFieldOp
                             , AddressOfOp
                             , AndOp
                             , AsrOp
                             , BoolToIntOp
                             , BpOp
                             , CallOutputOp
                             , CallOutputSSAOp
                             , CallParamOp
                             , CallParamSSAOp
                             , CallSSAOp
                             , CallUntypedOp
                             , CallUntypedSSAOp
                             , CeilOp
                             , CmpEOp
                             , CmpNeOp
                             , CmpSgeOp
                             , CmpSgtOp
                             , CmpSleOp
                             , CmpSltOp
                             , CmpUgeOp
                             , CmpUgtOp
                             , CmpUleOp
                             , CmpUltOp
                             , ConstOp
                             , ConstPtrOp
                             , DivsDpOp
                             , DivsOp
                             , DivuDpOp
                             , DivuOp
                             , ExternPtrOp
                             , FabsOp
                             , FaddOp
                             , FcmpEOp
                             , FcmpGeOp
                             , FcmpGtOp
                             , FcmpLeOp
                             , FcmpLtOp
                             , FcmpNeOp
                             , FcmpOOp
                             , FcmpUoOp
                             , FdivOp
                             , FloatConstOp
                             , FloatConvOp
                             , FloatToIntOp
                             , FloorOp
                             , FmulOp
                             , FnegOp
                             , FreeVarSlotOp
                             , FreeVarSlotSSAOp
                             , FsqrtOp
                             , FsubOp
                             , FtruncOp
                             , GotoOp
                             , IfOp
                             , ImportOp
                             , IntToFloatOp
                             , IntrinsicOp
                             , IntrinsicSSAOp
                             , JumpOp
                             , JumpToOp
                             , LoadOp
                             , LoadSSAOp
                             , LoadStructOp
                             , LoadStructSSAOp
                             , LowPartOp
                             , LslOp
                             , LsrOp
                             , MemPhiOp
                             , ModsDpOp
                             , ModsOp
                             , ModuDpOp
                             , ModuOp
                             , MulOp
                             , MulsDpOp
                             , MuluDpOp
                             , NegOp
                             , NoretOp
                             , NotOp(NotOp) --TODO: do this for every xOp...
                             , OrOp
                             , RetHintOp
                             , RetOp
                             , RlcOp
                             , RolOp
                             , RorOp
                             , RoundToIntOp
                             , RrcOp
                             , SbbOp
                             , SetVarAliasedFieldOp
                             , SetVarAliasedOp
                             , SetVarFieldOp
                             , SetVarOp
                             , SetVarSSAFieldOp
                             , SetVarSSAOp
                             , SetVarSplitOp
                             , SetVarSplitSSAOp
                             , StoreStructOp
                             , StoreStructSSAOp
                             , SubOp
                             , SxOp
                             , SyscallOp
                             , SyscallSSAOp
                             , SyscallUntypedOp
                             , SyscallUntypedSSAOp
                             , TailcallOp
                             , TailcallSSAOp
                             , TailcallUntypedOp
                             , TailcallUntypedSSAOp
                             , TestBitOp
                             , TrapOp
                             , UndefOp
                             , UnimplOp
                             , XorOp
                             , ZxOp
                             , OperationSize
                             , SSAVariable
                             , HasSize
                             , HasOp
                             , HasCondition
                             , HasDest
                             , HasFunc
                             , HasLeft
                             , HasOffset
                             , HasParams
                             , HasRight
                             , HasVar
                             , HasSrc
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

import Hinja.Function (Function)

newtype CtxIndex = CtxIndex Int
  deriving (Eq, Ord, Show, Num)

type Symbol = Text

data PilVar = PilVar
  { _symbol :: Symbol
  , _func :: Maybe Function
  , _ctxIndex :: Maybe CtxIndex
  , _mapsTo :: Set SSAVariableRef
  } deriving (Eq, Ord, Show, Generic)

newtype Converter a = Converter { _runConverter :: StateT Ctx IO a}
  deriving (Functor, Applicative, Monad, MonadState Ctx, MonadIO)

runConverter :: Converter a -> Ctx -> IO (a, Ctx)
runConverter m s = flip runStateT s $ _runConverter m

data SSAVariableRef = SSAVariableRef
  { _var :: SSAVariable
  , _func :: Maybe Function
  , _ctxIndex :: Maybe CtxIndex
  } deriving (Eq, Ord, Show, Generic)

--- maybe should use higher kinded types for this
data MExpression = MExpression
  { _size :: OperationSize
  , _op :: Maybe (ExprOp MExpression)
  } deriving (Eq, Ord, Show, Generic)

data Expression = Expression
  { _size :: OperationSize
  , _op :: ExprOp Expression
  } deriving (Eq, Ord, Show, Generic)

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
--    | VAR (VarOp expr)
    | VAR_ALIASED (VarAliasedOp expr)
    | VAR_ALIASED_FIELD (VarAliasedFieldOp expr)
--    | VAR_FIELD (VarFieldOp expr)
    | VAR_PHI (VarPhiOp expr)
--    | VAR_SPLIT (VarSplitOp expr)
    | VAR_SPLIT (VarSplitOp expr)
    | VAR (VarOp expr)
    | VAR_FIELD (VarFieldOp expr)
    | XOR (XorOp expr)
    | ZX (ZxOp expr)

    | StrCmp (StrCmpOp expr)
    | StrNCmp (StrNCmpOp expr)
    | MemCmp (MemCmpOp expr)
    | ConstStr (ConstStrOp expr)
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-------- Ops that use MLIL SSA Vars must be changed to use PilVars

data VarOp expr = VarOp
    { _varOpSrc :: PilVar
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data VarFieldOp expr = VarFieldOp
    { _varFieldOpSrc :: PilVar
    , _varFieldOpOffset :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data VarAliasedOp expr = VarAliasedOp
    { _varAliasedOpSrc :: PilVar
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data VarAliasedFieldOp expr = VarAliasedFieldOp
    { _varAliasedFieldOpSrc :: PilVar
    , _varAliasedFieldOpOffset :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data VarPhiOp expr = VarPhiOp
    { _varPhiOpDest :: PilVar
    , _varPhiOpSrc :: [PilVar]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data VarSplitOp expr = VarSplitOp
    { _varSplitSSAOpHigh :: PilVar
    , _varSplitSSAOpLow :: PilVar
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

--TODO: address_of and address_of_field
---------------

data CallDest expr = CallConstPtr (ConstPtrOp expr)
                   | CallExpr expr
                   | CallExprs (Set expr)

data CallOp expr = CallOp
  { _dest :: ConstPtrOp expr
  , _name :: Maybe Text
  , _params :: [expr]
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data StrCmpOp expr = StrCmpOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data StrNCmpOp expr = StrNCmpOp
    { _left :: expr
    , _right :: expr
    , _n :: Int
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data MemCmpOp expr = MemCmpOp
    { _left :: expr
    , _right :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data ConstStrOp expr = ConstStrOp
    { _value :: Text
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-----------------------
--- types

data TypedExpression = TypedExpression
  { _exprType :: Type
  , _size :: OperationSize
  , _op :: ExprOp Expression
  } deriving (Eq, Ord, Show, Generic)

newtype BitVecType = BitVecType
  { _width :: Int
  } deriving (Eq, Ord, Show, Generic)

data IntType = IntType
  { _width :: Int
  , _signed :: Bool
  } deriving (Eq, Ord, Show, Generic)

data FloatType = FloatType
  { _width :: Int
  } deriving (Eq, Ord, Show, Generic)

data ArrayType = ArrayType
  { _capacity :: Int
  , _elemType :: Type
  } deriving (Eq, Ord, Show, Generic)

data PtrType = PtrType
  { _width :: Int
  , _pointeeType :: Type
  } deriving (Eq, Ord, Show, Generic)

data FieldType = FieldType
  { _offset :: Int
  , _fieldType :: Type
  } deriving (Eq, Ord, Show, Generic)

data StructType = StructType
  { _size :: Int
  , _fields :: [Type]
  } deriving (Eq, Ord, Show, Generic)

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
          deriving (Eq, Ord, Show, Generic)
           
type TypeEnv = Map PilVar Type

------

data Ctx = Ctx
  { _func :: Maybe Function
  , _ctxIndex :: Maybe CtxIndex
  , _definedVars :: Set PilVar
  , _typeEnv :: TypeEnv
  } deriving (Eq, Ord, Show)

data StackOffset = StackOffset
  { _func :: Function
  , _ctxIndex :: CtxIndex
  , _offset :: Int
  } deriving (Eq, Ord, Show, Generic)

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

data StoreOp expr = StoreOp
    { _addr :: expr
    , _value :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data ConstraintOp expr = ConstraintOp
    { _condition :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data UnimplMemOp expr = UnimplMemOp
    { _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

type Stmt = Statement Expression

data Statement expr = Def (DefOp expr)
                    | Constraint (ConstraintOp expr)
                    | Store (StoreOp expr)
                    | UnimplInstr
                    | UnimplMem (UnimplMemOp expr)
                    | Undef
                    | Nop
                    | Annotation Text
                    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

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
$(makeFieldsNoPrefix ''StackOffset)
$(makeFieldsNoPrefix ''Storage)

$(makeFieldsNoPrefix ''DefOp)
$(makeFieldsNoPrefix ''StoreOp)
$(makeFieldsNoPrefix ''UnimplMemOp)
$(makeFieldsNoPrefix ''ConstraintOp)


