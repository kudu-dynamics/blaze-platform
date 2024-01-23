module Blaze.Types.Pil
  ( module Exports
  , module Blaze.Types.Pil
  ) where

import Blaze.Prelude hiding (Symbol, Type)
import Blaze.Types.Function (Function)
import Blaze.Types.Pil.Ops as Exports
import Blaze.Types.Pil.Common as Exports


data ExprOp expr
    = ADC (AdcOp expr)
    | ADD (AddOp expr)
    | ADD_WILL_CARRY (AddWillCarryOp expr)
    | ADD_WILL_OVERFLOW (AddWillOverflowOp expr)
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
    | POPCNT (PopcntOp expr)
    | RLC (RlcOp expr)
    | ROL (RolOp expr)
    | ROR (RorOp expr)
    | ROUND_TO_INT (RoundToIntOp expr)
    | RRC (RrcOp expr)
    | SBB (SbbOp expr)
    | SUB (SubOp expr)
    | SUB_WILL_OVERFLOW (SubWillOverflowOp expr)
    | SX (SxOp expr)
    | TEST_BIT (TestBitOp expr)
    | UNIMPL Text
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
    | ExternPtr ExternPtrOp
    | ConstStr (ConstStrOp expr)
    | ConstFuncPtr ConstFuncPtrOp
    | STACK_LOCAL_ADDR (StackLocalAddrOp expr)
    | UPDATE_VAR (UpdateVarOp expr)
    -- memory address specifier ops
    | FIELD_ADDR (FieldAddrOp expr)  -- struct
    | CONST_BOOL (ConstBoolOp expr)
    | UNIT
    | ARRAY_ADDR (ArrayAddrOp expr)
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

data Expression = Expression
  { size :: Size Expression
  , op :: ExprOp Expression
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

widthToSize :: Bits -> Size a
widthToSize x = Size $ toBytes x

sizeToWidth :: Size a -> Bits
sizeToWidth (Size x) = toBits x

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

data VarJoinOp expr = VarJoinOp
  { high :: PilVar
  , low :: PilVar
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

--TODO: address_of and address_of_field
---------------

data CallOp expr = CallOp
  { dest :: CallDest expr
  , name :: Maybe Text
  , args :: [expr]
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

data ExtractOp expr = ExtractOp
  { src :: expr
  , offset :: ByteOffset
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

data ConstFuncPtrOp = ConstFuncPtrOp
  { address :: Address
  , symbol :: Maybe Symbol
  } deriving (Eq, Ord, Show, Generic, Hashable, ToJSON, FromJSON)

data ExternPtrOp = ExternPtrOp
  { address :: Address
  , offset :: ByteOffset
  , symbol :: Maybe Symbol
  } deriving (Eq, Ord, Show, Generic, Hashable, ToJSON, FromJSON)

{- HLINT ignore StackLocalAddrOp "Use newtype instead of data" -}
data StackLocalAddrOp expr = StackLocalAddrOp
  { stackOffset :: StackOffset
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

data UpdateVarOp expr = UpdateVarOp
  { dest :: expr
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

data ArrayAddrOp expr = ArrayAddrOp
  { base :: expr
  , index :: expr
  , stride :: Word64
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
  , args :: [expr]
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

data ExitContextOp expr = ExitContextOp
  { leavingCtx :: Ctx
  , returningToCtx :: Ctx
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

data DefPhiOp expr = DefPhiOp
  { dest :: PilVar
  , src :: [PilVar]
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

data DefMemPhiOp expr = DefMemPhiOp
  { destMemory :: Int64
  , srcMemory :: [Int64]
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

{- HLINT ignore BranchCondOp "Use newtype instead of data" -}
data BranchCondOp expr = BranchCondOp
  { cond :: expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

{- HLINT ignore JumpOp "Use newtype instead of data" -}
data JumpOp expr = JumpOp
  { dest :: expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

{- HLINT ignore JumpToOp "Use newtype instead of data" -}
data JumpToOp expr = JumpToOp
  { dest :: expr
  , targets :: [Address]
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

newtype RetOp expr = RetOp
  { value :: expr
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (Hashable)
  deriving newtype (ToJSON, FromJSON)

data TailCallOp expr = TailCallOp
  { dest :: CallDest expr
  , name :: Maybe Text
  , args :: [expr]
  , ret :: Maybe (PilVar, Size Expression)
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

data GotoOp expr = GotoOp

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
  | DefMemPhi (DefMemPhiOp expr)
  | BranchCond (BranchCondOp expr)
  -- | Direct jump to 'dest', but statically we know 'dest' will be a member of
  -- 'targets' (probably from a jump table)
  | Jump (JumpOp expr)
  -- | Direct jump to 'dest'
  | JumpTo (JumpToOp expr)
  | Ret (RetOp expr)
  | NoRet
  | Exit
  | TailCall (TailCallOp expr)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

data MappedStatement a expr = MappedStatement
  { ref :: a
  , stmt :: Statement expr
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

instance Bifunctor MappedStatement where
  bimap f g (MappedStatement a b) = MappedStatement (f a) (g <$> b)
  
type MappedStmt a = MappedStatement a Expression

data CallStatement = CallStatement
  { stmt :: Statement Expression
  , callOp :: CallOp Expression
  , args :: [Expression]
  , resultVar :: Maybe PilVar
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

mkCallStatement :: Stmt -> Maybe CallStatement
mkCallStatement stmt' = case stmt' of
  Call callOp' ->
    Just $ CallStatement stmt' callOp' (callOp' ^. #args) Nothing
  Def (DefOp resultVar' (Expression _sz (CALL callOp'))) ->
    Just $ CallStatement stmt' callOp' (callOp' ^. #args) (Just resultVar')
  TailCall tc ->
    Just $ CallStatement stmt' callOp' (tc ^. #args) Nothing
    where
      callOp' = CallOp (tc ^. #dest) (tc ^. #name) (tc ^. #args)
  _ ->
    Nothing

getCallDest :: CallStatement -> CallDest Expression
getCallDest = view (#callOp . #dest)

mkCallDest :: HasField' "op" expr (ExprOp expr) => expr -> CallDest expr
mkCallDest x = case x ^. #op of
  (CONST_PTR c) ->
    CallAddr
      . ConstFuncPtrOp (fromIntegral $ c ^. #constant)
      $ Nothing
  _ -> CallExpr x

------------------------

mkFieldOffsetExprAddr :: Expression -> Int64 -> Expression
mkFieldOffsetExprAddr addrExpr offst =
  Expression
    (addrExpr ^. #size)
    ( FIELD_ADDR . FieldAddrOp addrExpr
        . fromIntegral
        $ offst
    )

data Access = In | Out | InOut | Unknown
  deriving (Enum, Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

-- | Parameter positions start at position 1.
newtype ParamPosition = ParamPosition Int
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

data PositionalParameter = PositionalParameter
  { var :: PilVar
  , access :: Access
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

data VarArgParameter
  = VarArgParameter
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

data FunctionInfo = FunctionInfo
  { name :: Symbol
  , start :: Address
  , params :: [PositionalParameter]
  , varparam :: Maybe VarArgParameter
  , locals :: [PilVar]
  , globals :: [PilVar]
  , result :: Maybe PilVar
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

{- | Used to reference a function by symbol and address.
 In particular, useful when the function body is unavailable
 or awkward to include.
-}
data FuncRef = FuncRef
  { name :: Symbol
  , start :: Address
  , params :: [PositionalParameter]
  , varparam :: Maybe VarArgParameter
  , hasResult :: Bool
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

{- | Used for type inference of functions to assign
 shared type variables to function parameters.
-}
data FuncVar expr
  = FuncParam (CallTarget expr) ParamPosition
  | FuncResult (CallTarget expr)
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

{- | A call target has an expression destination
 along with parameter and result information.
 Used when a function cannot be resolved (e.g., indirect call)
 or when it simple has not been resolved.
 A CallTarget can be used to group call sites that share
 the same call destination and expectations around
 call arguments and results.
-}
newtype CallTarget expr = CallTarget
  { dest :: CallDest expr
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

newtype ResultInfo = ResultInfo
  { name :: Text }
  deriving (Eq, Show, Generic)
  deriving anyclass (Hashable)

-- TODO: Consider extending this to support
--       the call expression as the result.
newtype CallResult = CallResult
  { var :: PilVar }

-- TODO: Remove this?
-- TODO: Should outputDest just be a single PilVar?
--       The list is a holdover from MLIL SSA
data CallStmt = CallStmt
  { stmt :: Stmt
  , address :: Address
  , index :: StmtIndex
  , size :: Size Expression
  , args :: [Expression]
  , dest :: CallDest Expression
  , outputDest :: [PilVar]
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass Hashable

-- TODO: Consider removing the CallConstPtr data constructor
--       as const ptrs can juse be an expression.
--       The purpose was to disambiguate between static
--       and dynamic call destinations, but perhaps this could
--       be represented in a better way?
-- TODO: Need a syscall dest?
-- | Represent possible call destinations found in a call statement.
-- The 'CallUnk' constructor is used in cases where recovering the call destination may
-- fail.
data CallDest expr = CallAddr ConstFuncPtrOp
                   | CallFunc Function
                   | CallExpr expr
                   | CallExprs [expr]
                   | CallExtern ExternPtrOp
                   | CallUnk
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable, ToJSON, FromJSON)

data CallSite = CallSite
  { caller :: Function
  , callStmt :: CallStmt
  , callDest :: CallDest Expression
  } deriving (Eq, Ord, Show, Generic)
