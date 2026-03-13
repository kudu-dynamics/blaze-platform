{-# OPTIONS_GHC -fno-warn-partial-fields #-}
{- HLINT ignore "Use newtype instead of data" -}

module Flint.Types.Analysis.Path.Matcher where

import Flint.Prelude hiding (sym, negate)
import qualified Flint.Types.Analysis.Path.Matcher.Func as M
import Flint.Types.Analysis (Taint, TaintPropagator)
import Flint.Types.Analysis.Path.Matcher.Primitives (PrimSpec, CallableWMI)
import Flint.Types.Symbol (Symbol)

import qualified Blaze.Pil.Display as Disp
import qualified Blaze.Pretty as Pretty
import Blaze.Types.Function (ExternFunction, Func)
import qualified Blaze.Types.Pil as Pil
import Blaze.Pil.Construct (ExprConstructor)
import qualified Blaze.Pil.Construct as C
import Blaze.Types.Pil (AddressableStatement, Size(Size))
import Blaze.Types.Pil.Checker (BitWidth, DeepSymType, InfoExpression)
import qualified Blaze.Types.Pil.Checker as Ch
import Blaze.Types.Pil.Solver (SolverResult(Sat))

import Control.Monad.Logic.Class
import Control.Monad.Logic (LogicT, runLogicT, observeManyT)
import qualified Control.Monad.Logic as L
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.SBV.Dynamic as Exports (CV)
import Data.String (IsString(fromString))


data CallDest expr
  = CallFunc M.Func
  | CallIndirect expr
  deriving (Eq, Ord, Show, Hashable, Generic)

data Statement expr
  = Def expr expr -- Def dst src; dst is always (Var PilVar) expr
  | Constraint expr
  | Store expr expr
  | EnterContext CtxPattern [expr]
  | ExitContext CtxPattern CtxPattern -- leavingCtx, returningToCtx
  -- | Call <return pat> <call dest> [<arg1>, <arg2>, ...]
  -- If return pat is Nothing, we don't care if it returns or doesn't.
  -- If return pat is `Just Wild` it only matches if something is returned.
  -- The arg list can be shorter than the actual arg count, but not longer
  | Call (Maybe expr) (CallDest expr) [expr]
  | BranchCond expr
  | Jump expr
  | Ret expr
  | NoRet
  -- These are sort of supported by Call, since it matches against any CallStatement
  -- | TailCall (CallDest expr) [expr]
  deriving (Eq, Ord, Show, Hashable, Generic)

-- | TODO: might need to make a `not` and `or` for CtxPattern
data CtxPattern
  = AnyCtx
  | BindCtx (Symbol Pil.Ctx) CtxPattern
  | Ctx (Maybe M.Func) (Maybe Pil.CtxId)
  deriving (Eq, Ord, Show, Hashable, Generic)

data StmtPattern
  = Stmt (Statement ExprPattern)
  -- | Tries to find "until" first, then makes sure "avoid" pattern doesn't
  -- occur on any of the statement that lead up to the "until"
  -- We must do it in this order so you can refer to exprs bound in the "until"
  -- when checking the "avoid"
  | AvoidUntil AvoidSpec
  -- | Once StmtPattern has matched, BoundExprs will be checked with the solver
  | Where StmtPattern [BoundExpr]
  | Necessarily StmtPattern [BoundExpr]
  | EndOfPath
  | Location (Symbol Address) StmtPattern
  -- | SubPrimitive <prim> <var patterns>
  -- Tries CallableWMI lookup (through call-sites) and inline pattern matching.
  -- Used for matching sub-patterns within a complex primitive.
  | SubPrimitive Prim (HashMap (Symbol Pil.Expression) ExprPattern)
  -- | CallsPrimitive <primSpec> <var patterns>
  -- Only checks CallableWMI cache (through call-sites). No inline fallback.
  | CallsPrimitive PrimSpec (HashMap (Symbol Pil.Expression) ExprPattern)
  | Star -- | Kleene star. Consumes any statements until next match
  | And StmtPattern StmtPattern -- | conjunction of two sequential patterns
  | Or StmtPattern StmtPattern -- | disjunction of two patterns
  | Good -- | stop trying to match, pattern is done
  | Bad
  deriving (Eq, Ord, Show, Hashable, Generic)


-- | Conjunction of sequential StmtPatterns. [] is assumed to be successful
ordered :: [StmtPattern] -> StmtPattern
ordered [] = Good
ordered [x] = x
ordered (x:xs) = x `And` ordered xs

orr :: [StmtPattern] -> StmtPattern
orr [] = Bad
orr [x] = x
orr (x:xs) = x `Or` orr xs


data BoundExprSize
  = ConstSize (Size Pil.Expression)
  | SizeOf (Symbol Pil.Expression)  -- looks up symbol to get size of expr
  deriving (Eq, Ord, Show, Hashable, Generic)

data BoundExpr
  = Bound (Symbol Pil.Expression) -- gets expression that has been bound with Bind
  | BoundExpr BoundExprSize (Pil.ExprOp BoundExpr)
  deriving (Eq, Ord, Show, Hashable, Generic)

class BoundVar a where
  bound :: Symbol Pil.Expression -> a

instance BoundVar BoundExpr where
  bound = Bound

instance ExprConstructor BoundExprSize BoundExpr where
  mkExpr = BoundExpr

instance Disp.NeedsParens BoundExpr where
  needsParens (Bound _) = False
  needsParens (BoundExpr _ op) = Disp.needsParens op

instance Pretty.Tokenizable BoundExpr where
  tokenize (Bound sym) = pure [Pretty.varToken Nothing ("?" <> cs sym)]
  tokenize (BoundExpr (ConstSize (Size size)) op) = Pretty.tokenizeExprOp Nothing op (Size size)
  tokenize (BoundExpr (SizeOf _) op) = Pretty.tokenizeExprOp Nothing op (Size 0)

-- | Text that can refer to variables bound during pattern matching
data BoundText
  = TextExpr (Symbol Pil.Expression)
  | PureText Text
  | CombineText BoundText BoundText
  | CaseContains BoundText [(Text, BoundText)]
  deriving (Eq, Ord, Show, Hashable, Generic)

instance Semigroup BoundText where
  a <> b = CombineText a b

instance IsString BoundText where
  fromString = PureText . cs

data ExprPattern
  = Expr (Pil.ExprOp ExprPattern)

  -- | Binds expr to Sym if pattern matches, or if Sym already exists,
  -- sees if equal to old sym val.
  -- You can nest more binds within the ExprPattern.
  | Bind (Symbol Pil.Expression) ExprPattern

  -- | Binds the byte width of an Expression as a const expr
  | BindWidth (Symbol Pil.Expression) ExprPattern

  -- | Matches prefix of var name, like "arg4" will match "arg4-7#1".
  -- Also matches against ConstFuncPtrs that a name.
  | Var (Symbol Pil.Expression)

  -- | Matches on vars that are parameters to the func
  | Param

  -- | Matches global addresses from data sections (.data and .bss)
  | GlobalAddr

  -- | Matches if its an immediate, like a const int, ptr, float, etc.
  | Immediate

  -- | Matches if ExprPattern matches somewhere inside expr
  | Contains ExprPattern
  -- | Matches if 'src' is involved in the definition of 'dst'
  -- | TaintedBy ExprPattern BoundExpr
  -- | Matches if the expression is tainted by a source
  | TaintedBy ExprPattern
  | Wild

  -- | Inequalities. These match on converse inequalities that mean the same thing,
  -- like `not (x != y)` would match `x .== y`, and `x < y` would match `y .> x`
  -- It also will work for signed or unsigned ints and floats.
  -- TODO: add explicit signed/unsigned variants if deemed important
  -- TODO: do we need NOT? and if so, do we want a thing that just says
  --       the expr needs to be true?
  | Cmp CmpType ExprPattern ExprPattern

  -- | This Or's together two expr patterns, so if the first fails, it checks snd.
  -- Not to be confused with the bitwise PIL.OR operator
  | OrPattern ExprPattern ExprPattern

  -- | Matches if the expr pattern inside doesn't match
  | NotPattern ExprPattern

  -- | Matches to see if expr is of a certain type
  | OfType TypePattern ExprPattern
  deriving (Eq, Ord, Show, Hashable, Generic)

data BitWidthPattern
  = ConstBitWidth Bits
  | AnyBitWidth
  | BindBitWidthAsExpr (Symbol Pil.Expression) BitWidthPattern
  deriving (Eq, Ord, Show, Hashable, Generic)

data LenPattern
  = ConstLen Word64
  | AnyLen
  | BindLenAsExpr (Symbol Pil.Expression) LenPattern
  deriving (Eq, Ord, Show, Hashable, Generic)

data PilTypePattern t
  = TBool
  | TChar {bitWidth :: BitWidthPattern}

  -- | for signed, Nothing means you don't care
  | TInt {bitWidth :: BitWidthPattern, signed :: Maybe Bool}
  | TFloat {bitWidth :: BitWidthPattern}
  | TBitVector {bitWidth :: BitWidthPattern}
  | TPointer {bitWidth :: BitWidthPattern, pointeeType :: t}
  | TCString {strLen :: LenPattern}
  | TArray {len :: LenPattern, elemType :: t}

  -- | The list in TRecord specifies fields that must match.
  -- As long as all of them match, it's ok. There can be extra fields
  -- in the actual type that are not matched.
  | TRecord [(BitWidthPattern, t)]
  | TUnit
  | TBottom
  | TFunction {retType :: t, params :: [t]}
  deriving (Eq, Ord, Show, Hashable, Generic)

data TypePattern
  = PilType (PilTypePattern TypePattern)
  | BoundType (Symbol DeepSymType) TypePattern
  | AnyType
  deriving (Eq, Ord, Show, Hashable, Generic)

class HasWildCard a where
  wild :: a

instance HasWildCard ExprPattern where
  wild = Wild

instance HasWildCard BitWidthPattern where
  wild = AnyBitWidth

instance HasWildCard LenPattern where
  wild = AnyLen

instance HasWildCard TypePattern where
  wild = AnyType

instance ExprConstructor () ExprPattern where
  mkExpr _ = Expr

(.||) :: ExprPattern -> ExprPattern -> ExprPattern
(.||) = OrPattern
infixr 3 .||

data CmpType
  = CmpE
  | CmpNE
  | CmpGT
  | CmpGE
  | CmpLT
  | CmpLE
  deriving (Eq, Ord, Show, Hashable, Generic)

(.==) :: ExprPattern -> ExprPattern -> ExprPattern
(.==) = Cmp CmpE
infix 4 .==

(./=) :: ExprPattern -> ExprPattern -> ExprPattern
(./=) = Cmp CmpNE
infix 4 ./=

(.<) :: ExprPattern -> ExprPattern -> ExprPattern
(.<) = Cmp CmpLT
infix 4 .<

(.<=) :: ExprPattern -> ExprPattern -> ExprPattern
(.<=) = Cmp CmpLE
infix 4 .<=

(.>) :: ExprPattern -> ExprPattern -> ExprPattern
(.>) = Cmp CmpGT
infix 4 .>

(.>=) :: ExprPattern -> ExprPattern -> ExprPattern
(.>=) = Cmp CmpGE
infix 4 .>=

data AvoidSpec = AvoidSpec
  { avoid :: StmtPattern
  , until :: StmtPattern
  } deriving (Eq, Ord, Show, Hashable, Generic)

-- | For converting callableWMIs map to nested version, so you can just look up all the findings
-- for a particular primspec.
asNestedMap
  :: forall a b c. (Hashable a, Hashable b)
  => HashMap (a, b) c
  -> HashMap a (HashMap b c)
asNestedMap
  = foldr (\ ((a, b), c) m -> HashMap.alter (addInnerMap b c) a m) HashMap.empty
  . HashMap.toList
  where
    addInnerMap :: b -> c -> Maybe (HashMap b c) -> Maybe (HashMap b c)
    addInnerMap b c (Just m) = Just $ HashMap.insert b c m
    addInnerMap b c Nothing = Just $ HashMap.singleton b c

-- | Converts to old version of the map that didn't inclde a Func in key.
-- Useful for test compatibility.
asOldCallableWMIsMap
  :: (Eq c, Hashable a, Hashable b)
  => HashMap (a, b) (HashSet c)
  -> HashMap a (HashSet c)
asOldCallableWMIsMap = HashMap.fromList
  . fmap (second $ HashSet.unions . HashMap.elems)
  . HashMap.toList
  . asNestedMap


--------- Primitives

data Prim = Prim
  { primType :: PrimSpec
  , stmtPattern :: StmtPattern
  } deriving (Eq, Ord, Show, Hashable, Generic)

type TypedExpr = InfoExpression (BitWidth, Maybe DeepSymType)
type TypedStmt = AddressableStatement TypedExpr

class HasAddress a where
  getAddress :: a -> Address

instance HasAddress (Pil.AddressableStatement a) where
  getAddress = view #addr

class HasDeepSymType a where
  getDeepSymType :: a -> Maybe DeepSymType

class IsStatement expr stmt | stmt -> expr where
  getStatement :: stmt -> Pil.Statement expr
  -- | Make a stmt that might just have default info for things like address
  mkDefStmt :: Pil.Statement expr -> stmt
  -- | Makes a stmt with same attrs (like address) as stmt
  mkStmtLike :: stmt -> Pil.Statement expr -> stmt
  asStmt :: stmt -> Pil.Stmt

asStmts :: (Functor f, IsStatement expr stmt) => f stmt -> f Pil.Stmt
asStmts = fmap asStmt

instance IsStatement Pil.Expression Pil.Stmt where
  getStatement = view #statement
  mkDefStmt = Pil.Stmt (intToAddr 0x0)
  mkStmtLike aStmt statement' = aStmt & #statement .~ statement'
  asStmt = identity

instance IsStatement (InfoExpression (BitWidth, Maybe DeepSymType)) (Pil.AddressableStatement (InfoExpression (BitWidth, Maybe DeepSymType))) where
  getStatement = view #statement
  mkDefStmt = Pil.Stmt (intToAddr 0x0)
  mkStmtLike aStmt statement' = aStmt & #statement .~ statement'
  asStmt = over #statement $ fmap asExpression

-- | This is a lot of trouble, but it's all so we can use the
-- same stuff to match regular Pil Expressions and ones with types
-- or ones with other info
-- Unfortunately, mkExpr requires using defaults for extra info
class Eq expr => IsExpression expr where
  getExprOp :: expr -> Pil.ExprOp expr
  getExprSize :: expr -> Pil.Size Pil.Expression
  asExpression :: expr -> Pil.Expression
  -- | Makes a new expr with the new expr op, but with all the
  -- other attributes of the old expr (like size, type, etc)
  mkExprLike :: expr -> Pil.ExprOp expr -> expr
  mkExprWithSize :: Pil.Size Pil.Expression -> Pil.ExprOp expr -> expr
  liftVar :: Pil.PilVar -> expr
  -- | lifts pilvar with same attrs as expr
  liftVarLike :: expr -> Pil.PilVar -> expr
  getType :: expr -> Maybe DeepSymType

instance IsExpression Pil.Expression where
  getExprOp = view #op
  getExprSize = view #size
  asExpression = identity
  mkExprLike x op = x & #op .~ op
  mkExprWithSize = Pil.Expression
  liftVar pv = Pil.Expression sz . Pil.VAR . Pil.VarOp $ pv
    where
      sz = coerce $ pv ^. #size
  liftVarLike x = Pil.Expression (x ^. #size) . Pil.VAR . Pil.VarOp
  getType = const Nothing

instance IsExpression (InfoExpression (BitWidth, Maybe DeepSymType)) where
  getExprOp = view #op
  getExprSize = fromIntegral . toBytes . view (#info . _1)
  asExpression x = Pil.Expression (getExprSize x) (asExpression <$> x ^. #op)
  mkExprLike x op = x & #op .~ op
  mkExprWithSize sz = Ch.InfoExpression (toBits . fromIntegral $ sz, Nothing)
  liftVar pv = mkExprWithSize (fromIntegral $ pv ^. #size) (Pil.VAR . Pil.VarOp $ pv)
  liftVarLike x = mkExprLike x . Pil.VAR . Pil.VarOp
  getType x = x ^. #info . _2
  
type StmtSolver stmt m = [stmt] -> m SolverResult

dummySolver :: Applicative m => StmtSolver stmt m
dummySolver _ = pure $ Sat HashMap.empty

data MatcherCtx stmt m = MatcherCtx
  { pathSolver :: StmtSolver stmt m
  , taintSet :: HashSet Taint
  , taintPropagators :: [TaintPropagator]
  -- , callablePrimitives :: HashMap (PrimSpec, Func) (HashSet CallableWMI)
  } deriving (Generic)
  
data MatcherState expr stmt = MatcherState
  { remaining :: [stmt]
  , boundSyms :: HashMap (Symbol Pil.Expression) expr
  , boundCtxSyms :: HashMap (Symbol Pil.Ctx) Pil.Ctx

  -- TODO: ensure that isomorphic DeepSymTypes are `==`
  -- The problem is the DSVar syms might not be the same,
  -- even if the recursive structure is the same
  , boundTypes :: HashMap (Symbol DeepSymType) DeepSymType
  
    -- The successfully parsed stmts, stored in reverse order
    -- possibly interleaved with user-made Assertions
  , parsedStmtsWithAssertions :: [stmt]
    -- | If the path has been checked with the solver,
    -- this holds possible solutions
  , solutions :: Maybe (HashMap Text CV)
  
  -- | Locations can be for an actual statement, or also for extern funcs
  -- like in the case of stdlib funcs
  , locations :: HashMap (Symbol Address) (Either ExternFunction Address)
  , callablePrimitives :: HashMap (PrimSpec, Func) (HashSet CallableWMI)
  } deriving (Eq, Ord, Show, Generic)

getCallableWMIs_
  :: PrimSpec
  -> Func
  -> HashMap (PrimSpec, Func) (HashSet CallableWMI)
  -> Maybe (HashSet CallableWMI)
getCallableWMIs_ prim fn = HashMap.lookup (prim, fn)

getCallableWMIs :: PrimSpec -> Func -> MatcherT expr stmt m (Maybe (HashSet CallableWMI))
getCallableWMIs prim fn = do
  cprimMap <- use #callablePrimitives
  return $ getCallableWMIs_ prim fn cprimMap

emptyMatcherState :: MatcherState expr stmt
emptyMatcherState = MatcherState [] HashMap.empty HashMap.empty HashMap.empty [] Nothing HashMap.empty HashMap.empty

dropStmt :: MatcherT expr stmt m ()
dropStmt = #remaining %= drop 1

newtype MatcherT expr stmt m a = MatcherT {
  _runMatcherT :: StateT (MatcherState expr stmt) (ReaderT (MatcherCtx stmt m) (LogicT m)) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadFail
    , MonadIO
    , Alternative
    , MonadLogic
    , MonadPlus
    , MonadState (MatcherState expr stmt)
    , MonadReader (MatcherCtx stmt m)
    )

instance MonadTrans (MatcherT expr stmt) where
  lift = MatcherT . lift . lift . lift

observeTMaybe :: Monad m => LogicT m a -> m (Maybe a)
observeTMaybe m = runLogicT m (\a _ -> return (Just a)) (return Nothing)

observeMatcherT
  :: Monad m
  => MatcherCtx stmt m
  -> MatcherState expr stmt
  -> MatcherT expr stmt m a
  -> m (Maybe (a, MatcherState expr stmt))
observeMatcherT ctx s (MatcherT m) = observeTMaybe . (`runReaderT` ctx) $ runStateT m s

observeManyMatcherT
  :: Monad m
  => MatcherCtx stmt m
  -> MatcherState expr stmt
  -> Int
  -> MatcherT expr stmt m a
  -> m [(a, MatcherState expr stmt)]
observeManyMatcherT ctx s maxResults (MatcherT m) = observeManyT maxResults . (`runReaderT` ctx) $ runStateT m s

addCallableWMI_
  :: CallableWMI
  -> HashMap (PrimSpec, Func) (HashSet CallableWMI)
  -> HashMap (PrimSpec, Func) (HashSet CallableWMI)
addCallableWMI_ cprim = HashMap.alter f (primType, cprim ^. #func)
  where
    primType = cprim ^. #prim

    f :: Maybe (HashSet CallableWMI) -> Maybe (HashSet CallableWMI)
    f Nothing = Just $ HashSet.singleton cprim
    f (Just s) = Just $ HashSet.insert cprim s
