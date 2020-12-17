{-# LANGUAGE TemplateHaskell #-}

module Blaze.Types.Pil
  ( -- module Exports,
    module Blaze.Types.Pil,
  )
where


import Blaze.Prelude hiding (Symbol, Type)
import Blaze.Types.CallGraph (Function)
import Blaze.Types.ClassyFields (HasSize(size))

import qualified Data.HashMap.Strict as HM

import Blaze.Types.Pil.Op.AdcOp (AdcOp)
import Blaze.Types.Pil.Op.AddOp (AddOp)
import Blaze.Types.Pil.Op.AddOverflowOp (AddOverflowOp)
import Blaze.Types.Pil.Op.AndOp (AndOp)
import Blaze.Types.Pil.Op.AsrOp (AsrOp)
import Blaze.Types.Pil.Op.BoolToIntOp (BoolToIntOp)
import Blaze.Types.Pil.Op.CeilOp (CeilOp)
import Blaze.Types.Pil.Op.CmpEOp (CmpEOp)
import Blaze.Types.Pil.Op.CmpNeOp (CmpNeOp)
import Blaze.Types.Pil.Op.CmpSgeOp (CmpSgeOp)
import Blaze.Types.Pil.Op.CmpSgtOp (CmpSgtOp)
import Blaze.Types.Pil.Op.CmpSleOp (CmpSleOp)
import Blaze.Types.Pil.Op.CmpSltOp (CmpSltOp)
import Blaze.Types.Pil.Op.CmpUgeOp (CmpUgeOp)
import Blaze.Types.Pil.Op.CmpUgtOp (CmpUgtOp)
import Blaze.Types.Pil.Op.CmpUleOp (CmpUleOp)
import Blaze.Types.Pil.Op.CmpUltOp (CmpUltOp)
import Blaze.Types.Pil.Op.ConstOp (ConstOp)
import Blaze.Types.Pil.Op.ConstFloatOp (ConstFloatOp)
import Blaze.Types.Pil.Op.ConstPtrOp (ConstPtrOp)
import Blaze.Types.Pil.Op.DivsOp (DivsOp)
import Blaze.Types.Pil.Op.DivsDpOp (DivsDpOp)
import Blaze.Types.Pil.Op.DivuOp (DivuOp)
import Blaze.Types.Pil.Op.DivuDpOp (DivuDpOp)
import Blaze.Types.Pil.Op.FabsOp (FabsOp)
import Blaze.Types.Pil.Op.FaddOp (FaddOp)
import Blaze.Types.Pil.Op.FcmpEOp (FcmpEOp)
import Blaze.Types.Pil.Op.FcmpGeOp (FcmpGeOp)
import Blaze.Types.Pil.Op.FcmpGtOp (FcmpGtOp)
import Blaze.Types.Pil.Op.FcmpLeOp (FcmpLeOp)
import Blaze.Types.Pil.Op.FcmpLtOp (FcmpLtOp)
import Blaze.Types.Pil.Op.FcmpNeOp (FcmpNeOp)
import Blaze.Types.Pil.Op.FcmpOOp (FcmpOOp)
import Blaze.Types.Pil.Op.FcmpUoOp (FcmpUoOp)
import Blaze.Types.Pil.Op.FdivOp (FdivOp)
import Blaze.Types.Pil.Op.FloatConvOp (FloatConvOp)
import Blaze.Types.Pil.Op.FloatToIntOp (FloatToIntOp)
import Blaze.Types.Pil.Op.FloorOp (FloorOp)
import Blaze.Types.Pil.Op.FmulOp (FmulOp)
import Blaze.Types.Pil.Op.FnegOp (FnegOp)
import Blaze.Types.Pil.Op.FsqrtOp (FsqrtOp)
import Blaze.Types.Pil.Op.FsubOp (FsubOp)
import Blaze.Types.Pil.Op.FtruncOp (FtruncOp)
import Blaze.Types.Pil.Op.ImportOp (ImportOp)
import Blaze.Types.Pil.Op.IntToFloatOp (IntToFloatOp)
import Blaze.Types.Pil.Op.LoadOp (LoadOp)
import Blaze.Types.Pil.Op.LowPartOp (LowPartOp)
import Blaze.Types.Pil.Op.LslOp (LslOp)
import Blaze.Types.Pil.Op.LsrOp (LsrOp)
import Blaze.Types.Pil.Op.ModsOp (ModsOp)
import Blaze.Types.Pil.Op.ModsDpOp (ModsDpOp)
import Blaze.Types.Pil.Op.ModuOp (ModuOp)
import Blaze.Types.Pil.Op.ModuDpOp (ModuDpOp)
import Blaze.Types.Pil.Op.MulOp (MulOp)
import Blaze.Types.Pil.Op.MulsDpOp (MulsDpOp)
import Blaze.Types.Pil.Op.MuluDpOp (MuluDpOp)
import Blaze.Types.Pil.Op.NegOp (NegOp)
import Blaze.Types.Pil.Op.NotOp (NotOp)
import Blaze.Types.Pil.Op.OrOp (OrOp)
import Blaze.Types.Pil.Op.RlcOp (RlcOp)
import Blaze.Types.Pil.Op.RolOp (RolOp)
import Blaze.Types.Pil.Op.RorOp (RorOp)
import Blaze.Types.Pil.Op.RoundToIntOp (RoundToIntOp)
import Blaze.Types.Pil.Op.RrcOp (RrcOp)
import Blaze.Types.Pil.Op.SbbOp (SbbOp)
import Blaze.Types.Pil.Op.SubOp (SubOp)
import Blaze.Types.Pil.Op.SxOp (SxOp)
import Blaze.Types.Pil.Op.TestBitOp (TestBitOp)
import Blaze.Types.Pil.Op.XorOp (XorOp)
import Blaze.Types.Pil.Op.ZxOp (ZxOp)


newtype StmtIndex = StmtIndex { _val :: Int }
  deriving(Eq, Ord, Show, Generic)
  deriving newtype Num
  deriving anyclass Hashable

type Symbol = Text

newtype OperationSize = OperationSize Bytes
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral, Generic)

instance Hashable OperationSize

newtype CtxIndex = CtxIndex Int
  deriving (Eq, Ord, Show, Generic)
  deriving newtype Num
  deriving anyclass Hashable

data Ctx = Ctx
  { _func :: Function
  , _ctxIndex :: CtxIndex
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass Hashable
$(makeFieldsNoPrefix ''Ctx)

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
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

data ExprOp expr
    = ADC (AdcOp expr)
    | ADD (AddOp expr)
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
    | CONST_FLOAT (ConstFloatOp expr)
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
    | UNIMPL Text
    | VAR_PHI (VarPhiOp expr)
    | VAR_JOIN (VarJoinOp expr)
    | VAR (VarOp expr)
    | VAR_FIELD (VarFieldOp expr)
    | XOR (XorOp expr)
    | ZX (ZxOp expr)

    | CALL (CallOp expr)

    | Extract (ExtractOp expr)
    | StrCmp (StrCmpOp expr)
    | StrNCmp (StrNCmpOp expr)
    | MemCmp (MemCmpOp expr)
    | ConstStr (ConstStrOp expr)
    | STACK_LOCAL_ADDR (StackLocalAddrOp expr)
    | UPDATE_VAR (UpdateVarOp expr)
    -- memory address specifier ops
    | FIELD_ADDR (FieldAddrOp expr)  -- struct
    | CONST_BOOL (ConstBoolOp expr)
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (ExprOp a)

data Expression = Expression
  { _size :: OperationSize
  , _op :: ExprOp Expression
  } 
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

-------- Ops that use MLIL SSA Vars must be changed to use PilVars
{- HLINT ignore VarOp "Use newtype instead of data" -}
data VarOp expr = VarOp
    { _varOpSrc :: PilVar
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (VarOp a)

data VarFieldOp expr = VarFieldOp
    { _varFieldOpSrc :: PilVar
    , _varFieldOpOffset :: ByteOffset
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (VarFieldOp a)

data VarPhiOp expr = VarPhiOp
    { _varPhiOpDest :: PilVar
    , _varPhiOpSrc :: [PilVar]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (VarPhiOp a)

data VarJoinOp expr = VarJoinOp
    { _varJoinOpHigh :: PilVar
    , _varJoinOpLow :: PilVar
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (VarJoinOp a)

--TODO: address_of and address_of_field
---------------
-- TODO: Consider removing the CallConstPtr data constructor
--       as const ptrs can juse be an expression.
--       The purpose was to disambiguate between static 
--       and dynamic call destinations, but perhaps this could
--       be represented in a better way?
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

{- HLINT ignore ConstStrOp "Use newtype instead of data" -}
data ConstStrOp expr = ConstStrOp
    { _value :: Text
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (ConstStrOp a)

{- HLINT ignore StackLocalAddrOp "Use newtype instead of data" -}
data StackLocalAddrOp expr = StackLocalAddrOp
    { _stackOffset :: StackOffset
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
instance Hashable a => Hashable (StackLocalAddrOp a)

data UpdateVarOp expr = UpdateVarOp
    { _dest :: PilVar
    , _offset :: ByteOffset
    , _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
instance Hashable a => Hashable (UpdateVarOp a)

data FieldAddrOp expr = FieldAddrOp
    { _baseAddr :: expr
    , _offset :: ByteOffset
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
instance Hashable a => Hashable (FieldAddrOp a)

{- HLINT ignore ConstBoolOp "Use newtype instead of data" -}
data ConstBoolOp expr = ConstBoolOp
    { _constant :: Bool
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
instance Hashable a => Hashable (ConstBoolOp a)

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

{- HLINT ignore FloatType "Use newtype instead of data" -}
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
typeEnvLookup v (TypeEnv env) = HM.lookup v env

instance Semigroup TypeEnv where
  (<>) (TypeEnv m1) (TypeEnv m2) = TypeEnv $ HM.unionWith (<>) m1 m2

instance Monoid TypeEnv where
  mempty = TypeEnv HM.empty

------

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

{- HLINT ignore ConstraintOp "Use newtype instead of data" -}
data ConstraintOp expr = ConstraintOp
    { _condition :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
instance Hashable a => Hashable (ConstraintOp a)

{- HLINT ignore UnimplMemOp "Use newtype instead of data" -}
data UnimplMemOp expr = UnimplMemOp
    { _src :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
instance Hashable a => Hashable (UnimplMemOp a)

{- HLINT ignore EnterContextOp "Use newtype instead of data" -}
data EnterContextOp expr = EnterContextOp
    { _ctx :: Ctx
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
instance Hashable a => Hashable (EnterContextOp a)

data ExitContextOp expr = ExitContextOp
    { _leavingCtx :: Ctx
    , _returningToCtx :: Ctx
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
instance Hashable a => Hashable (ExitContextOp a)

data DefPhiOp expr = DefPhiOp
    { _dest :: PilVar
    , _src :: [PilVar]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
instance Hashable a => Hashable (DefPhiOp a)

type Stmt = Statement Expression

data Statement expr
  = Def (DefOp expr)
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
  | DefPhi (DefPhiOp expr)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass Hashable

data CallStatement
  = CallStatement
      { _stmt :: Statement Expression,
        _callOp :: CallOp Expression
      }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

mkCallStatement :: Stmt -> Maybe CallStatement
mkCallStatement stmt = case stmt of
  Call callOp -> 
    Just $ CallStatement stmt callOp
  Def (DefOp _ (Expression _sz (CALL callOp))) ->
    Just $ CallStatement stmt callOp
  _ -> 
    Nothing

getCallDest :: CallStatement -> CallDest Expression
getCallDest callStmt =  _dest (_callOp callStmt :: CallOp Expression)

$(makeFields ''VarOp)
$(makeFields ''VarFieldOp)
$(makeFields ''VarPhiOp)
$(makeFields ''VarJoinOp)

$(makeFieldsNoPrefix ''StackLocalAddrOp)
$(makeFieldsNoPrefix ''FieldAddrOp)
$(makeFieldsNoPrefix ''UpdateVarOp)
$(makeFieldsNoPrefix ''ConstBoolOp)

$(makeFieldsNoPrefix ''PilVar)
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
$(makeFieldsNoPrefix ''StackOffset)
$(makeFieldsNoPrefix ''Storage)

$(makeFieldsNoPrefix ''CallStatement)

$(makeFieldsNoPrefix ''DefOp)
$(makeFieldsNoPrefix ''StoreOp)
$(makeFieldsNoPrefix ''UnimplMemOp)
$(makeFieldsNoPrefix ''ConstraintOp)
$(makeFieldsNoPrefix ''DefPhiOp)

$(makeFieldsNoPrefix ''Expression)
$(makePrisms ''ExprOp)
$(makePrisms ''Type)
$(makePrisms ''Statement)

$(makeFieldsNoPrefix ''AdcOp)
$(makeFieldsNoPrefix ''AddOp)
$(makeFieldsNoPrefix ''AddOverflowOp)
$(makeFieldsNoPrefix ''AndOp)
$(makeFieldsNoPrefix ''AsrOp)
$(makeFieldsNoPrefix ''BoolToIntOp)
$(makeFieldsNoPrefix ''CeilOp)
$(makeFieldsNoPrefix ''CmpEOp)
$(makeFieldsNoPrefix ''CmpNeOp)
$(makeFieldsNoPrefix ''CmpSgeOp)
$(makeFieldsNoPrefix ''CmpSgtOp)
$(makeFieldsNoPrefix ''CmpSleOp)
$(makeFieldsNoPrefix ''CmpSltOp)
$(makeFieldsNoPrefix ''CmpUgeOp)
$(makeFieldsNoPrefix ''CmpUgtOp)
$(makeFieldsNoPrefix ''CmpUleOp)
$(makeFieldsNoPrefix ''CmpUltOp)
$(makeFieldsNoPrefix ''ConstOp)
$(makeFieldsNoPrefix ''ConstFloatOp)
$(makeFieldsNoPrefix ''ConstPtrOp)
$(makeFieldsNoPrefix ''DivsOp)
$(makeFieldsNoPrefix ''DivsDpOp)
$(makeFieldsNoPrefix ''DivuOp)
$(makeFieldsNoPrefix ''DivuDpOp)
$(makeFieldsNoPrefix ''FabsOp)
$(makeFieldsNoPrefix ''FaddOp)
$(makeFieldsNoPrefix ''FcmpEOp)
$(makeFieldsNoPrefix ''FcmpGeOp)
$(makeFieldsNoPrefix ''FcmpGtOp)
$(makeFieldsNoPrefix ''FcmpLeOp)
$(makeFieldsNoPrefix ''FcmpLtOp)
$(makeFieldsNoPrefix ''FcmpNeOp)
$(makeFieldsNoPrefix ''FcmpOOp)
$(makeFieldsNoPrefix ''FcmpUoOp)
$(makeFieldsNoPrefix ''FdivOp)
$(makeFieldsNoPrefix ''FloatConvOp)
$(makeFieldsNoPrefix ''FloatToIntOp)
$(makeFieldsNoPrefix ''FloorOp)
$(makeFieldsNoPrefix ''FmulOp)
$(makeFieldsNoPrefix ''FnegOp)
$(makeFieldsNoPrefix ''FsqrtOp)
$(makeFieldsNoPrefix ''FsubOp)
$(makeFieldsNoPrefix ''FtruncOp)
$(makeFieldsNoPrefix ''ImportOp)
$(makeFieldsNoPrefix ''IntToFloatOp)
$(makeFieldsNoPrefix ''LoadOp)
$(makeFieldsNoPrefix ''LowPartOp)
$(makeFieldsNoPrefix ''LslOp)
$(makeFieldsNoPrefix ''LsrOp)
$(makeFieldsNoPrefix ''ModsOp)
$(makeFieldsNoPrefix ''ModsDpOp)
$(makeFieldsNoPrefix ''ModuOp)
$(makeFieldsNoPrefix ''ModuDpOp)
$(makeFieldsNoPrefix ''MulOp)
$(makeFieldsNoPrefix ''MulsDpOp)
$(makeFieldsNoPrefix ''MuluDpOp)
$(makeFieldsNoPrefix ''NegOp)
$(makeFieldsNoPrefix ''NotOp)
$(makeFieldsNoPrefix ''OrOp)
$(makeFieldsNoPrefix ''RlcOp)
$(makeFieldsNoPrefix ''RolOp)
$(makeFieldsNoPrefix ''RorOp)
$(makeFieldsNoPrefix ''RoundToIntOp)
$(makeFieldsNoPrefix ''RrcOp)
$(makeFieldsNoPrefix ''SbbOp)
$(makeFieldsNoPrefix ''SubOp)
$(makeFieldsNoPrefix ''SxOp)
$(makeFieldsNoPrefix ''TestBitOp)
$(makeFieldsNoPrefix ''XorOp)
$(makeFieldsNoPrefix ''ZxOp)

------------------------

mkFieldOffsetExprAddr :: Expression -> Int64 -> Expression
mkFieldOffsetExprAddr addrExpr offst =
  Expression
    (addrExpr ^. size)
    ( FIELD_ADDR . FieldAddrOp addrExpr
        . fromIntegral
        $ offst
    )


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

-- mkCallDest :: HasOp expr (ExprOp expr) => expr -> CallDest expr
mkCallDest :: HasOp Expression (ExprOp Expression) => Expression -> CallDest Expression
mkCallDest x = case x ^. op of
  (CONST_PTR c) -> CallConstPtr c
  _ -> CallExpr x
