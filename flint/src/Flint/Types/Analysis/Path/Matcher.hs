{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Flint.Types.Analysis.Path.Matcher where

import Flint.Prelude hiding (sym, negate)
import Flint.Types.Analysis (Taint(..))
import qualified Flint.Types.Analysis.Path.Matcher.Func as M
import Flint.Types.Analysis.Path.Matcher.Primitives (PrimSpec, CallableWMI)
import Flint.Types.Symbol (Symbol)

import qualified Blaze.Pil.Display as Disp
import qualified Blaze.Pretty as Pretty
import Blaze.Types.Function (ExternFunction, Func)
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil (Size(Size))
import Blaze.Types.Pil.Summary (CodeSummary)
import Blaze.Pil.Construct (ExprConstructor)
import qualified Blaze.Pil.Construct as C
import Blaze.Types.Pil.Solver (SolverResult)

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
  -- | Eventually StmtPattern
  -- | Avoid StmtPattern
  | AvoidUntil AvoidSpec
  | AnyOne [StmtPattern]  -- One match succeeds. [] immediately succeeeds.
  | Unordered [StmtPattern] -- matches all, but in no particular order.
  | Ordered [StmtPattern] -- matches all. Good for grouping and scoping Where bounds.
  | Neighbors [StmtPattern] -- sequential neighbors with no unmatching stmts between
  -- | Once StmtPattern has matched, BoundExprs will be checked with the solver
  | Where StmtPattern [BoundExpr]
  | Necessarily StmtPattern [BoundExpr]
  | EndOfPath
  | Location (Symbol Address) StmtPattern
  -- | Primitive <prefix for bound vars> <primtype>
  | Primitive PrimSpec (HashMap (Symbol Pil.Expression) ExprPattern)
  deriving (Eq, Ord, Show, Hashable, Generic)

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

  -- | Matches prefix of var name, like "arg4" will match "arg4-7#1".
  -- Also matches against ConstFuncPtrs that a name.
  | Var (Symbol Pil.Expression)

  -- | Matches on vars that are parameters to the func
  | Param

  -- | Matches if its an immediate, like a const int, ptr, float, etc.
  | Immediate

  -- | Matches if ExprPattern matches somewhere inside expr
  | Contains ExprPattern
  -- | Matches if 'src' is involved in the definition of 'dst'
  | TaintedBy ExprPattern BoundExpr
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
  deriving (Eq, Ord, Show, Hashable, Generic)

instance ExprConstructor () ExprPattern where
  mkExpr _ = Expr

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

type StmtSolver m = [Pil.Stmt] -> m SolverResult

data MatcherState m = MatcherState
  { remainingStmts :: [Pil.Stmt]
  , boundSyms :: HashMap (Symbol Pil.Expression) Pil.Expression
  , boundCtxSyms :: HashMap (Symbol Pil.Ctx) Pil.Ctx
  , avoids :: HashMap AvoidSpec [Pil.Stmt]
  -- The successfully parsed stmts, stored in reverse order
  -- possibly interleaved with user-made Assertions
  , parsedStmtsWithAssertions :: [Pil.Stmt]
  , taintSet :: HashSet Taint
  , solveStmts :: StmtSolver m
  -- | If the path has been checked with the solver, this holds possible solutions
  , solutions :: Maybe (HashMap Text CV)
  , locations :: HashMap (Symbol Address) (Either ExternFunction Address)
  -- TODO: this doesn't really belong here because it won't be modified
  , callablePrimitives :: HashMap (PrimSpec, Func) (HashSet CallableWMI)
  } deriving Generic

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

getCallableWMIs_
  :: PrimSpec
  -> Func
  -> HashMap (PrimSpec, Func) (HashSet CallableWMI)
  -> Maybe (HashSet CallableWMI)
getCallableWMIs_ prim fn = HashMap.lookup (prim, fn)

getCallableWMIs :: Monad m => PrimSpec -> Func -> MatcherT m (Maybe (HashSet CallableWMI))
getCallableWMIs prim fn = do
  cprimMap <- use #callablePrimitives
  return $ getCallableWMIs_ prim fn cprimMap

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

-- TODO: probably should make a useful error that can pass up bad BoundExpr conversions
-- and solver errors
newtype MatcherT m a = MatcherT {
  _runMatcherT :: ExceptT () (StateT (MatcherState m) m) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadError ()
    , MonadIO
    , MonadState (MatcherState m)
    , Alternative
    )

instance MonadTrans MatcherT where
  lift = MatcherT . lift . lift

runMatcher_ :: MatcherT m a -> MatcherState m -> m (Either () a, MatcherState m)
runMatcher_ action s
  = flip runStateT s
  . runExceptT
  . _runMatcherT
  $ action

data MatcherResult
  = Match [Pil.Stmt]
  | NoMatch
  deriving (Eq, Ord, Show, Hashable, Generic)

data PathPrep = PathPrep
  { untouchedStmts :: [Pil.Stmt]
  , stmts :: [Pil.Stmt]
  , taintSet :: HashSet Taint
  , codeSummary :: CodeSummary
  } deriving (Eq, Ord, Show, Generic)

--------- Primitives

data Prim = Prim
  { primType :: PrimSpec
  , stmtPattern :: [StmtPattern]
  } deriving (Eq, Ord, Show, Hashable, Generic)
