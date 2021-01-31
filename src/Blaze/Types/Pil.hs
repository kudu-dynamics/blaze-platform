module Blaze.Types.Pil
  ( module Exports
  , module Blaze.Types.Pil
  ) where

import Blaze.Prelude hiding (Symbol, Type)
import Blaze.Types.Pil.Ops as Exports
import Blaze.Types.Pil.Common as Exports

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
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

data Expression = Expression
  { size :: OperationSize
  , op :: ExprOp Expression
  } 
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)


-- TODO: Move these into the PIL op generator
-------- Ops that use MLIL SSA Vars must be changed to use PilVars
{- HLINT ignore VarOp "Use newtype instead of data" -}
data VarOp expr = VarOp
  { src :: PilVar
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

data VarFieldOp expr = VarFieldOp
  { src :: PilVar
  , offset :: ByteOffset
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

data VarPhiOp expr = VarPhiOp
  { dest :: PilVar
  , src :: [PilVar]
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

data VarJoinOp expr = VarJoinOp
  { high :: PilVar
  , low :: PilVar
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

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
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

data CallOp expr = CallOp
  { dest :: CallDest expr
  , name :: Maybe Text
  , params :: [expr]
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

data ExtractOp expr = ExtractOp
  { src :: expr
  , offset :: Int64
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

data StrCmpOp expr = StrCmpOp
  { left :: expr
  , right :: expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

data StrNCmpOp expr = StrNCmpOp
  { left :: expr
  , right :: expr
  , len :: Int
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

data MemCmpOp expr = MemCmpOp
  { left :: expr
  , right :: expr
  , size :: Bytes
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

{- HLINT ignore ConstStrOp "Use newtype instead of data" -}
data ConstStrOp expr = ConstStrOp
  { value :: Text
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

{- HLINT ignore StackLocalAddrOp "Use newtype instead of data" -}
data StackLocalAddrOp expr = StackLocalAddrOp
  { stackOffset :: StackOffset
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

data UpdateVarOp expr = UpdateVarOp
  { dest :: PilVar
  , offset :: ByteOffset
  , src :: expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

data FieldAddrOp expr = FieldAddrOp
  { baseAddr :: expr
  , offset :: ByteOffset
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

{- HLINT ignore ConstBoolOp "Use newtype instead of data" -}
data ConstBoolOp expr = ConstBoolOp
  { constant :: Bool
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)


---- Statements

data DefOp expr = DefOp
  { var :: PilVar
  , value :: expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

data StoreOp expr = StoreOp
  { addr :: expr
  , value :: expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

{- HLINT ignore ConstraintOp "Use newtype instead of data" -}
data ConstraintOp expr = ConstraintOp
  { condition :: expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

{- HLINT ignore UnimplMemOp "Use newtype instead of data" -}
data UnimplMemOp expr = UnimplMemOp
  { src :: expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

{- HLINT ignore EnterContextOp "Use newtype instead of data" -}
data EnterContextOp expr = EnterContextOp
  { ctx :: Ctx
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

data ExitContextOp expr = ExitContextOp
  { leavingCtx :: Ctx
  , returningToCtx :: Ctx
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

data DefPhiOp expr = DefPhiOp
  { dest :: PilVar
  , src :: [PilVar]
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

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
  deriving anyclass (Hashable, ToJSON, FromJSON)

data CallStatement = CallStatement
  { stmt :: Statement Expression
  , callOp :: CallOp Expression
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

mkCallStatement :: Stmt -> Maybe CallStatement
mkCallStatement stmt' = case stmt' of
  Call callOp' ->
    Just $ CallStatement stmt' callOp'
  Def (DefOp _ (Expression _sz (CALL callOp'))) ->
    Just $ CallStatement stmt' callOp'
  _ ->
    Nothing

mkCallDest :: HasField' "op" expr (ExprOp expr) => expr -> CallDest expr
mkCallDest x = case x ^. #op of
  (CONST_PTR c) -> CallConstPtr c
  _ -> CallExpr x

getCallDest :: CallStatement -> CallDest Expression
getCallDest = view (#callOp . #dest)

------------------------

mkFieldOffsetExprAddr :: Expression -> Int64 -> Expression
mkFieldOffsetExprAddr addrExpr offst =
  Expression
    (addrExpr ^. #size)
    ( FIELD_ADDR . FieldAddrOp addrExpr
        . fromIntegral
        $ offst
    )

