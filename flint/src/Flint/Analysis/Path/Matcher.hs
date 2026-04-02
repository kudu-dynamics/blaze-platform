module Flint.Analysis.Path.Matcher
  ( module Flint.Analysis.Path.Matcher
  , module M
  ) where

-- | This module is an interpreter for the pattern DSL for the original matcher

import Flint.Prelude hiding (sym, negate, until, Location)

import qualified Flint.Analysis.Path.Matcher.Primitives as Prim
import qualified Flint.Analysis.Path.Matcher.Logic.Combinators as C
import Flint.Analysis.Path.Matcher.Logic.Combinators (good, bad, insist, (<|||>))
import Flint.Types.Analysis.Path.Matcher.Primitives (CallableWMI, FuncVarExpr, PrimSpec)
-- import Flint.Types.Analysis (Parameter(..), Taint(..), TaintPropagator(..))
import Flint.Analysis.Path.Matcher.Taint
import Flint.Types.Analysis.Path.Matcher -- (MatcherState, MatcherCtx, MatcherT, StmtSolver, IsExpression, IsStatement, HasAddress)
import Flint.Types.Analysis.Path.Matcher.PathPrep
import qualified Flint.Types.Analysis.Path.Matcher as M
import Flint.Types.Analysis.Path.Matcher.Func (Func(..))
import Flint.Types.Symbol (Symbol)

import Blaze.Pil.Eval (evalPilArithmeticExpr)
import Blaze.Pretty (pretty')
import qualified Blaze.Types.Pil as Pil
import qualified Blaze.Types.Function as BFunc
import Blaze.Types.Pil (Size)
import Blaze.Types.Pil.Checker as Ch
import Blaze.Types.Pil.Solver (SolverResult(Sat, Unsat))

import Control.Monad.Logic.Class (lnot, once)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Text.Regex.TDFA ((=~))
import qualified Data.Vector as V
import Blaze.Types.Pil.PilType as PT


------------------------------------------
-- stateful versions of combinators

-- | runs a parser that is not stateful (like from the Combinators module)
-- inside the state monad, such that it consumes the statements
runAsStateful
  :: ([stmt] -> MatcherT expr stmt m (a, [stmt]))
  -> MatcherT expr stmt m a
runAsStateful parser = do
  stmts <- use #remaining
  (r, remaining) <- parser stmts
  #remaining .= remaining
  return r

parseUntil
  :: Monad m
  => (stmt -> MatcherT expr stmt m a)
  -> MatcherT expr stmt m a
parseUntil = runAsStateful . C.parseUntil

-- | Next stmt must match, or this fails
parseNext
  :: Monad m
  => (stmt -> MatcherT expr stmt m a)
  -> MatcherT expr stmt m a
parseNext = runAsStateful . C.parseNext

avoidUntil
  :: Monad m
  => (b -> MatcherT expr stmt m ()) -- avoid
  -> MatcherT expr stmt m b -- until
  -> MatcherT expr stmt m b
avoidUntil avoid until = do
  stBeforeUntil <- get
  x <- until
  stAfterUntil <- get
  -- Reset state and try to match `avoid` on stmts parsed by `until`
  do -- put stBeforeUntil
     let stmtsBefore = stBeforeUntil ^. #remaining
         stmtsAfter = stAfterUntil ^. #remaining
         parsedStmts = take (length stmtsBefore - length stmtsAfter) stmtsBefore
     #remaining .= parsedStmts
     lnot $ avoid x
     put stAfterUntil
  return x

bind_
  :: (Eq a, Monad m)
  => Lens' (MatcherState expr stmt) (HashMap (Symbol b) a)
  -> Symbol b
  -> a
  -> MatcherT expr stmt m ()
bind_ lens' sym x = do
  bsyms <- use lens'
  case HashMap.lookup sym bsyms of
    Just x' -> insist $ x  == x'
    Nothing -> lens' %= HashMap.insert sym x

-- | Binds a sym to an expression. Or, if the sym already exists,
-- it checks to see if this expression matches the first.
bind
  :: (IsExpression expr, Monad m)
  => Symbol Pil.Expression
  -> expr
  -> MatcherT expr stmt m ()
bind  sym x = do
  bsyms <- use #boundSyms
  case HashMap.lookup sym bsyms of
    Just x' -> insist $ sortaEqual x x'
    Nothing -> #boundSyms %= HashMap.insert sym x
-- bind = bind_ #boundSyms

sortaEqual
  :: (IsExpression expr1, IsExpression expr2)
  => expr1
  -> expr2
  -> Bool
sortaEqual a b = case (asExpression a, asExpression b) of
  (Pil.Expression _ (Pil.STACK_LOCAL_ADDR stackOp), Pil.Expression _ (Pil.VAR varOp)) ->
    Just (stackOp ^. #stackOffset . #offset) == (fromIntegral <$> varOp ^? #src . #location . #_StackMemory)
  (Pil.Expression _ (Pil.VAR varOp), Pil.Expression _ (Pil.STACK_LOCAL_ADDR stackOp)) ->
    Just (stackOp ^. #stackOffset . #offset) == (fromIntegral <$> varOp ^? #src . #location . #_StackMemory)
  (x, y) -> x == y

bindCtx :: Monad m => Symbol Pil.Ctx -> Pil.Ctx -> MatcherT expr stmt m ()
bindCtx = bind_ #boundCtxSyms

bindType :: Monad m => Symbol DeepSymType -> DeepSymType -> MatcherT expr stmt m ()
bindType = bind_ #boundTypes

matchFuncPatWithFunc :: Monad m => Func -> BFunc.Function -> MatcherT expr stmt m ()
matchFuncPatWithFunc (FuncName name) func = insist
  $ funcNameMatches name (func ^. #name)
  || maybeFuncNameMatches name (func ^? #symbol . #_Just . #_symbolName)
matchFuncPatWithFunc (FuncNames names) func = insist
  $ funcNameSetMatches names (func ^. #name)
  || maybeFuncNameSetMatches names (func ^? #symbol . #_Just . #_symbolName)
matchFuncPatWithFunc (FuncAddr addr) func = insist $ addr == func ^. #address
matchFuncPatWithFunc (FuncNameRegex rpat) func = insist
  $ funcNameRegexMatches rpat (func ^. #name)
  || maybeFuncNameRegexMatches rpat (func ^? #symbol . #_Just . #_symbolName)

regexIsIn :: Text -> Text -> Bool
regexIsIn a b = b =~ a

normalizeFuncName :: Text -> Text
normalizeFuncName = Prim.cleanFuncName

funcNameMatches :: Text -> Text -> Bool
funcNameMatches expected actual =
  normalizeFuncName expected == normalizeFuncName actual

maybeFuncNameMatches :: Text -> Maybe Text -> Bool
maybeFuncNameMatches expected = maybe False (funcNameMatches expected)

funcNameSetMatches :: HashSet Text -> Text -> Bool
funcNameSetMatches expected actual =
  any (`funcNameMatches` actual) (HashSet.toList expected)

maybeFuncNameSetMatches :: HashSet Text -> Maybe Text -> Bool
maybeFuncNameSetMatches expected = maybe False (funcNameSetMatches expected)

funcNameRegexMatches :: Text -> Text -> Bool
funcNameRegexMatches pat actual =
  regexIsIn pat actual || regexIsIn pat (normalizeFuncName actual)

maybeFuncNameRegexMatches :: Text -> Maybe Text -> Bool
maybeFuncNameRegexMatches pat = maybe False (funcNameRegexMatches pat)

matchCtx :: Monad m => M.CtxPattern -> Pil.Ctx -> MatcherT expr stmt m ()
matchCtx pat ctx = case pat of
  M.AnyCtx -> good
  M.BindCtx sym ctxPat -> do
    matchCtx ctxPat ctx
    bindCtx sym ctx
  M.Ctx mFuncPat mCtxId -> do
    case mFuncPat of
      Nothing -> good
      Just funcPat -> matchFuncPatWithFunc funcPat $ ctx ^. #func
    case mCtxId of
      Nothing -> good
      Just x -> insist $ x == ctx ^. #ctxId

-- | Tries to absorb a "not" into a bool expression.
-- For instance, `(not (x == y))` will become `(x != y)`,
-- `(not (x < y))` will become `(x >= y)`, and `(not (not x))`
-- will become `x`, and `(not x)` will just remain `(not x)`
-- if Bool is true, then try to negate the expr
absorbNots_ :: IsExpression expr => Bool -> expr -> expr
-- The True case means an outer "Not" has already been found
absorbNots_ True expr = case getExprOp expr of
  -- (not (not x)) = x
  Pil.NOT (Pil.NotOp x) -> absorbNots_ False x

  Pil.CMP_E (Pil.CmpEOp a b) -> mkCmp . Pil.CMP_NE $ Pil.CmpNeOp a b
  Pil.CMP_NE (Pil.CmpNeOp a b) -> mkCmp . Pil.CMP_E $ Pil.CmpEOp a b

  Pil.CMP_SGE (Pil.CmpSgeOp a b) -> mkCmp . Pil.CMP_SLT $ Pil.CmpSltOp a b
  Pil.CMP_SGT (Pil.CmpSgtOp a b) -> mkCmp . Pil.CMP_SLE $ Pil.CmpSleOp a b
  Pil.CMP_SLE (Pil.CmpSleOp a b) -> mkCmp . Pil.CMP_SGT $ Pil.CmpSgtOp a b
  Pil.CMP_SLT (Pil.CmpSltOp a b) -> mkCmp . Pil.CMP_SGE $ Pil.CmpSgeOp a b

  Pil.CMP_UGE (Pil.CmpUgeOp a b) -> mkCmp . Pil.CMP_ULT $ Pil.CmpUltOp a b
  Pil.CMP_UGT (Pil.CmpUgtOp a b) -> mkCmp . Pil.CMP_ULE $ Pil.CmpUleOp a b
  Pil.CMP_ULE (Pil.CmpUleOp a b) -> mkCmp . Pil.CMP_UGT $ Pil.CmpUgtOp a b
  Pil.CMP_ULT (Pil.CmpUltOp a b) -> mkCmp . Pil.CMP_UGE $ Pil.CmpUgeOp a b

  Pil.FCMP_E (Pil.FcmpEOp a b) -> mkCmp . Pil.FCMP_NE $ Pil.FcmpNeOp a b
  Pil.FCMP_NE (Pil.FcmpNeOp a b) -> mkCmp . Pil.FCMP_E $ Pil.FcmpEOp a b

  Pil.FCMP_GE (Pil.FcmpGeOp a b) -> mkCmp . Pil.FCMP_LT $ Pil.FcmpLtOp a b
  Pil.FCMP_GT (Pil.FcmpGtOp a b) -> mkCmp . Pil.FCMP_LE $ Pil.FcmpLeOp a b
  Pil.FCMP_LE (Pil.FcmpLeOp a b) -> mkCmp . Pil.FCMP_GT $ Pil.FcmpGtOp a b
  Pil.FCMP_LT (Pil.FcmpLtOp a b) -> mkCmp . Pil.FCMP_GE $ Pil.FcmpGeOp a b

  -- Put the NOT back on because it can't be absorbed
  _ -> mkCmp . Pil.NOT $ Pil.NotOp expr
  where
    mkCmp = mkExprLike expr
absorbNots_ False expr = case getExprOp expr of
  Pil.NOT (Pil.NotOp x) -> absorbNots_ True x
  _ -> expr

absorbNots :: IsExpression expr => expr -> expr
absorbNots = absorbNots_ False

negate :: IsExpression expr => expr -> expr
negate x = mkExprLike x . Pil.NOT $ Pil.NotOp x

matchCmp
  :: ( Monad m
     , IsExpression expr
     )
  => M.CmpType
  -> M.ExprPattern
  -> M.ExprPattern
  -> expr
  -> MatcherT expr stmt m ()
matchCmp cmpType patA patB expr = case cmpType of
  M.CmpE -> case op of
    Pil.CMP_E (Pil.CmpEOp a b) -> once $ bimatch a b <|> bimatch b a
    Pil.FCMP_E (Pil.FcmpEOp a b) -> once $ bimatch a b <|> bimatch b a
    _ -> bad
  M.CmpNE -> case op of
    Pil.CMP_NE (Pil.CmpNeOp a b) -> once $ bimatch a b <|> bimatch b a
    Pil.FCMP_NE (Pil.FcmpNeOp a b) -> once $ bimatch a b <|> bimatch b a
    _ -> bad

  M.CmpGT -> case op of
    Pil.CMP_SGT (Pil.CmpSgtOp a b) -> bimatch a b
    Pil.CMP_UGT (Pil.CmpUgtOp a b) -> bimatch a b
    Pil.CMP_SLT (Pil.CmpSltOp a b) -> bimatch b a
    Pil.CMP_ULT (Pil.CmpUltOp a b) -> bimatch b a

    Pil.FCMP_GT (Pil.FcmpGtOp a b) -> bimatch a b
    Pil.FCMP_LT (Pil.FcmpLtOp a b) -> bimatch b a
    _ -> bad

  M.CmpGE -> case op of
    Pil.CMP_SGE (Pil.CmpSgeOp a b) -> bimatch a b
    Pil.CMP_UGE (Pil.CmpUgeOp a b) -> bimatch a b
    Pil.CMP_SLE (Pil.CmpSleOp a b) -> bimatch b a
    Pil.CMP_ULE (Pil.CmpUleOp a b) -> bimatch b a

    Pil.FCMP_GE (Pil.FcmpGeOp a b) -> bimatch a b
    Pil.FCMP_LE (Pil.FcmpLeOp a b) -> bimatch b a
    _ -> bad

  M.CmpLT -> case op of
    Pil.CMP_SLT (Pil.CmpSltOp a b) -> bimatch a b
    Pil.CMP_ULT (Pil.CmpUltOp a b) -> bimatch a b
    Pil.CMP_SGT (Pil.CmpSgtOp a b) -> bimatch b a
    Pil.CMP_UGT (Pil.CmpUgtOp a b) -> bimatch b a

    Pil.FCMP_LT (Pil.FcmpLtOp a b) -> bimatch a b
    Pil.FCMP_GT (Pil.FcmpGtOp a b) -> bimatch b a
    _ -> bad

  M.CmpLE -> case op of
    Pil.CMP_SLE (Pil.CmpSleOp a b) -> bimatch a b
    Pil.CMP_ULE (Pil.CmpUleOp a b) -> bimatch a b
    Pil.CMP_SGE (Pil.CmpSgeOp a b) -> bimatch b a
    Pil.CMP_UGE (Pil.CmpUgeOp a b) -> bimatch b a

    Pil.FCMP_LE (Pil.FcmpLeOp a b) -> bimatch a b
    Pil.FCMP_GE (Pil.FcmpGeOp a b) -> bimatch b a
    _ -> bad

  where
    bimatch a b = matchExpr patA a >> matchExpr patB b
    exprWithAbsorbedNots = absorbNots expr
    op = getExprOp exprWithAbsorbedNots

matchExprOp
  :: ( Monad m
     , IsExpression expr
     )
  => Pil.ExprOp M.ExprPattern
  -> Pil.ExprOp expr
  -> MatcherT expr stmt m ()
matchExprOp opPat op = do
  insist $ void opPat == void op
  traverse_ (uncurry matchExpr) $ zip (toList opPat) (toList op)

matchExpr
  :: ( Monad m
     , IsExpression expr
     )
  => M.ExprPattern
  -> expr
  -> MatcherT expr stmt m ()
matchExpr pat expr = case pat of
  M.Bind sym xpat -> do
    matchExpr xpat expr
    -- success
    bind sym expr

  M.Bound sym -> do
    bsyms <- use #boundSyms
    case HashMap.lookup sym bsyms of
      Nothing -> bad
      Just x' -> insist $ sortaEqual x' expr

  M.BindWidth sym xpat -> do
    matchExpr xpat expr
    let szExpr = mkExprLike expr
          . Pil.CONST
          $ Pil.ConstOp (fromIntegral (fromByteBased $ getExprSize expr :: Bytes))
    -- success
    bind sym szExpr

  M.Var prefixOfName -> case getExprOp expr of
    Pil.VAR (Pil.VarOp pv) -> insist . Text.isPrefixOf (cs prefixOfName) $ pv ^. #symbol
    Pil.ConstFuncPtr (Pil.ConstFuncPtrOp _addr (Just symb)) -> do
      insist $ Text.isPrefixOf (cs prefixOfName) symb
    _ -> bad
  M.Param -> case getExprOp expr of
    Pil.VAR (Pil.VarOp pv) -> insist $ pv ^. #isParam
    _ -> bad
  M.GlobalAddr -> case getExprOp expr of
    Pil.GLOBAL_PTR _ -> good
    _ -> bad
  M.Immediate -> maybe bad (const good) . evalPilArithmeticExpr $ asExpression expr
  M.Contains xpat -> do
    matchExpr xpat expr
      <|> C.orr (matchExpr (M.Contains xpat) <$> toList (getExprOp expr))

  -- we we really be passing both src and dst pats
  --but TaintedBy really only needs what source to look at right?
-- M.TaintedBy _dstPat _src -> do
  M.TaintedBy scrpat -> do
    -- TODO: match on destination pattern against expression
    --matchExpr dstPat expr
    -- TODO: get the taint set
    --ts <- view  #taintSet
    -- TODO: resolve bound source expressio
    -- just check taint from any source actually
    -- srcExpr <- resolveBoundExpr src
    -- TODO Eventually will check if source taints any bits
    --insist $ isTainted ts (asExpression expr)
    -- CLEANMEUP
    ts <- view #taintSet
    tps <- view #taintPropagators
    let expr' = asExpression expr
    case scrpat of
      -- find what expr is tainted by globalss
      M.GlobalAddr -> insist $ isTaintedBySrc tps containsGlobalPtr ts expr'
      _ -> insist $ isTainted tps ts expr'

  M.Wild -> good
  M.Expr xop -> case (xop, getExprOp expr) of
    -- the ADD pattern should match FIELD_ADDR since both do addition
    (Pil.ADD addPat, Pil.FIELD_ADDR fieldOp) -> do
      matchExpr (addPat ^. #left) (fieldOp ^. #baseAddr)
      let offsetValue = fromIntegral (fieldOp ^. #offset)
          offsetExpr = mkExprLike expr (Pil.CONST $ Pil.ConstOp offsetValue)
      matchExpr (addPat ^. #right) offsetExpr
    (_, op) -> matchExprOp xop op
  M.Cmp cmpType patA patB -> matchCmp cmpType patA patB expr
  M.OrPattern patA patB -> matchExpr patA expr <|||> matchExpr patB expr
  M.NotPattern patA -> do
    st <- get
    lnot $ matchExpr patA expr
    put st
  M.OfType tpat xpat -> do
    matchExpr xpat expr
    -- if there is inferred type, we just fail
    maybe bad (matchType tpat) $ getType expr

newtype InfinitePilType = InfinitePilType { unInfiniteType :: PilType InfinitePilType }
  deriving (Eq, Ord, Read, Show, Generic)

-- | Lazily converts a deep sym type to an infinitely nested type.
toInfiniteType :: DeepSymType -> InfinitePilType
toInfiniteType = toInfiniteType' HashMap.empty

toInfiniteType' :: HashMap PT.Sym (PT.PilType DeepSymType) -> DeepSymType -> InfinitePilType
toInfiniteType' m = \case
  Ch.DSVar s -> InfinitePilType
    . maybe PT.TUnit (fmap $ toInfiniteType' m)
    $ HashMap.lookup s m
  Ch.DSRecursive s pt -> InfinitePilType $ toInfiniteType' (HashMap.insert s pt m) <$> pt
  Ch.DSType pt -> InfinitePilType $ toInfiniteType' m <$> pt

-- | This attempts to match on the exact inferred type.
-- We are not matching on things that *could* be the type.
-- For example, if you want to match TChar, but it's a 1 byte TBitVector,
-- the match will fail. We could reconsider this later.
matchType
  :: ( IsExpression expr
     , Monad m
     )
  => M.TypePattern
  -> DeepSymType
  -> MatcherT expr stmt m ()
matchType = matchType' HashMap.empty

-- | DeepSymTypes can have a recursive self-referential context.
-- We have to keep track of this so we can properly expand type as we need it.
matchType'
  :: ( IsExpression expr
     , Monad m
     )
  => HashMap PT.Sym (PT.PilType DeepSymType) -- | recursiveContext
  -> M.TypePattern
  -> DeepSymType
  -> MatcherT expr stmt m ()
matchType' recCtx tpat dst = case tpat of
  M.AnyType -> good
  M.BoundType tsym tpat' -> do
    matchType' recCtx tpat' dst
    bindType tsym dst
  M.PilType ptPat -> case dst of
    -- | If it's a recursive type var, look it up and run matchType' on it
    Ch.DSVar s -> maybe bad (matchType' recCtx tpat . Ch.DSType)
      $ HashMap.lookup s recCtx

    -- | If it is declaring a recursive type, store it in the recCtx,
    -- then run the type matching pattern on the type.
    Ch.DSRecursive s pt -> matchType' (HashMap.insert s pt recCtx) tpat
      $ Ch.DSType pt

    Ch.DSType pt -> case (ptPat, pt) of
      (M.TBool, PT.TBool) -> good

      (M.TChar bwPat, PT.TChar mbits) -> do
        matchBitWidth bwPat mbits

      (M.TInt bwPat signPat, PT.TInt mbits msign) -> do
        insist $ isNothing signPat || signPat == msign
        matchBitWidth bwPat mbits

      (M.TFloat bwPat, PT.TFloat mbits) -> do
        matchBitWidth bwPat mbits

      (M.TBitVector bwPat, PT.TBitVector mbits) -> do
        matchBitWidth bwPat mbits

      (M.TPointer bwPat pointeePat, PT.TPointer mbits pointee) -> do
        matchBitWidth bwPat mbits
        matchType' recCtx pointeePat pointee

      (M.TCString lenPat, PT.TCString mlen) -> do
        matchLen lenPat mlen

      (M.TArray lenPat elemPat, PT.TArray mlen elemt) -> do
        matchLen lenPat mlen
        matchType' recCtx elemPat elemt

      (M.TRecord fieldPats, PT.TRecord fields) -> do
        matchRecordFields recCtx fieldPats . HashMap.toList $ fields

      (M.TUnit, PT.TUnit) -> good

      (M.TFunction retPat paramsPats, PT.TFunction ret params) -> do
        matchType' recCtx retPat ret
        if length paramsPats > length params
          then bad
          else forM_ (zip paramsPats params) $ uncurry (matchType' recCtx)

      (_, _) -> bad

-- | Matches each record field pattern to an actual record field.
-- Two record field patterns are not allowed to match the same record field.
-- We return all possible combinations of record field matches.
matchRecordFields
  :: forall expr stmt m.
     ( IsExpression expr
     , Monad m
     )
  => HashMap PT.Sym (PT.PilType DeepSymType) -- | recursiveContext
  -> [(M.BitWidthPattern, M.TypePattern)]
  -> [(BitOffset, DeepSymType)]
  -> MatcherT expr stmt m ()
matchRecordFields recCtx fieldPats fields = C.orr perms
  where
    fieldPatsAsParsers :: [(BitOffset, DeepSymType) -> MatcherT expr stmt m ()]
    fieldPatsAsParsers = foreach fieldPats $ \(bwPat, fieldPat) (bw, fieldType) -> do
      matchBitWidth bwPat (Just $ fromIntegral bw)
      matchType' recCtx fieldPat fieldType

    -- Get all permutations of fieldPats (all different orderings)
    -- Then check them each against normal list of fields
    perms = foreach (permutations fieldPatsAsParsers) $ \parsers -> do
      void $ C.parseThroughList parsers fields

-- | This is the default size to give constructed expressions,
-- particularly for BindBitWidthAsExpr and BindLenAsExpr.
-- These exprs might be used in generated PIL to use for SMT,
-- which means the size might be incompatible with other comparisons.
-- We need to "repair" step where sizes are extended to be compatible.
defaultExprSize :: Pil.Size Pil.Expression
defaultExprSize = 4

matchBitWidth
  :: ( Monad m
     , IsExpression expr
     )
  => BitWidthPattern
  -> Maybe Bits
  -> MatcherT expr stmt m ()
matchBitWidth bwpat mbits = case bwpat of
  M.ConstBitWidth n -> maybe bad (insist . (== n)) mbits
  M.AnyBitWidth -> good
  M.BindBitWidthAsExpr s pat -> case mbits of
    Nothing -> bad -- can't bind to expr if we don't know it
    Just bits -> do
      matchBitWidth pat mbits
      bind s . mkExprWithSize defaultExprSize . Pil.CONST . Pil.ConstOp . fromIntegral $ bits

matchLen
  :: ( Monad m
     , IsExpression expr
     , Integral a
     )
  => LenPattern
  -> Maybe a
  -> MatcherT expr stmt m ()
matchLen lenPat mlen = case lenPat of
  M.ConstLen n -> maybe bad (insist . (== n) . fromIntegral) mlen
  M.AnyLen -> good
  M.BindLenAsExpr s pat -> case mlen of
    Nothing -> bad -- can't bind to expr if we don't know it
    Just len -> do
      matchLen pat mlen
      bind s . mkExprWithSize defaultExprSize . Pil.CONST . Pil.ConstOp . fromIntegral $ len

data CallStatement expr stmt = CallStatement
  { stmt :: stmt
  , callOp :: Pil.CallOp expr
  , args :: [expr]
  , resultVar :: Maybe Pil.PilVar
  , callExpr :: Maybe expr -- | needed to get the type of the return, but TailCall and Call stmts have no associated expr (which is probably OK because they don't return anything)
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

-- TODO: this should probably go in Blaze so they stay in sync
-- But then we'd need to have something like `IsExpression` and `IsStatement`
-- in blaze, which we don't need for anything else.
mkCallStatement
  :: (IsExpression expr, IsStatement expr stmt)
  => stmt
  -> Maybe (CallStatement expr stmt)
mkCallStatement stmt' = case getStatement stmt' of
  Pil.Call callOp' ->
    Just $ CallStatement stmt' callOp' (callOp' ^. #args) Nothing Nothing
  Pil.Def (Pil.DefOp resultVar' expr) -> case getExprOp expr of
    Pil.CALL callOp' -> Just $ CallStatement stmt' callOp' (callOp' ^. #args) (Just resultVar') (Just expr)
    _ -> Nothing
  Pil.TailCall tc ->
    Just $ CallStatement stmt' callOp' (tc ^. #args) Nothing Nothing
    where
      callOp' = Pil.CallOp (tc ^. #dest) (tc ^. #args)
  _ ->
    Nothing

matchStmt
  :: ( Monad m
     , HasAddress stmt
     , IsStatement expr stmt
     , IsExpression expr
     )
  => M.Statement M.ExprPattern
  -> stmt
  -> MatcherT expr stmt m ()
matchStmt sPat stmt = case sPat of
  M.Def destPat srcPat -> case statement of
    Pil.Def (Pil.DefOp pv expr) -> do
      let pvExpr = liftVarLike expr pv
      matchExpr destPat pvExpr
      matchExpr srcPat expr
    _ -> bad

  M.Constraint expr -> case statement of
    Pil.Constraint (Pil.ConstraintOp condExpr) -> do
      matchExpr expr condExpr
    _ -> bad

  M.Store addrPat valPat -> case statement of
    Pil.Store (Pil.StoreOp addrExpr valExpr) -> do
      matchExpr addrPat addrExpr
      matchExpr valPat valExpr
    _ -> bad

  M.EnterContext ctxPat argPats -> case statement of
    Pil.EnterContext (Pil.EnterContextOp ctx argExprs) -> do
      matchCtx ctxPat ctx
      if length argPats > length argExprs
        then bad
        else do
          traverse_ (uncurry matchExpr) $ zip argPats argExprs
    _ -> bad

  M.ExitContext leavingCtxPat returningToCtxPat -> case statement of
    Pil.ExitContext (Pil.ExitContextOp leavingCtx returningToCtx) -> do
      matchCtx leavingCtxPat leavingCtx
      matchCtx returningToCtxPat returningToCtx
    _ -> bad

  M.Call mResultPat callDestPat argPats -> case statement of
    Pil.EnterContext (Pil.EnterContextOp ctx argExprs) -> do
      case callDestPat of
        -- TODO: it would be nice to somehow still match on expanded indirect calls,
        --   but i'm not sure how, because the dest expr gets replaced by a concrete func
        M.CallIndirect _ -> bad
        M.CallFunc funcPat -> matchFuncPatWithFunc funcPat $ ctx ^. #func
      if length argPats > length argExprs
        then bad
        else traverse_ (uncurry matchExpr) $ zip argPats argExprs
      let ctxPat = M.Ctx (Just . FuncAddr $ ctx ^. #func . #address) (Just $ ctx ^. #ctxId)
          endPat = case mResultPat of
            Nothing -> M.orr
              [ M.EndOfPath
              , M.Stmt $ M.ExitContext ctxPat M.AnyCtx
              ]
            Just retPat -> M.ordered
              [ M.Stmt $ M.Ret retPat
              , M.Stmt $ M.ExitContext ctxPat M.AnyCtx
              ]
          -- fastForward until we reach the end of the call
      -- -- Retire current call statement
      -- retireStmt stmt
      matchPattern $ M.ordered [M.Star, endPat]

    _ -> case mkCallStatement stmt of
      Nothing -> bad
      Just (CallStatement _ callOp argExprs mResultVar mCallExpr) -> do
        matchCallDest callDestPat $ callOp ^. #dest
        case (mResultPat, mResultVar) of
          (Nothing, _) -> good
          (Just resultPat, Just resultVar) -> do
            let pvExpr = maybe liftVar liftVarLike mCallExpr resultVar
            matchExpr resultPat pvExpr
          _ -> bad
      -- It's ok if there are less arg pats than there are args
      -- I don't think we should make them match, since the lifter gets
      -- the number of args wrong sometimes
      -- But it should fail if there are less args than patterns.
        if length argPats > length argExprs
          then bad
          else traverse_ (uncurry matchExpr) $ zip argPats argExprs

  M.BranchCond condPat -> case statement of
    Pil.BranchCond (Pil.BranchCondOp condExpr) -> do
      matchExpr condPat condExpr
    _ -> bad

  M.Jump destPat -> case statement of
    Pil.Jump (Pil.JumpOp destExpr) -> do
      matchExpr destPat destExpr
    _ -> bad

  M.Ret valPat -> case statement of
    Pil.Ret (Pil.RetOp valExpr) -> do
      matchExpr valPat valExpr
    _ -> bad

  M.NoRet -> case statement of
    Pil.NoRet -> good
    _ -> bad

  where
    statement = getStatement stmt

matchCallDest
  :: ( IsExpression expr
     , Monad m
     )
  => M.CallDest M.ExprPattern
  -> Pil.CallDest expr
  -> MatcherT expr stmt m ()
matchCallDest pat cdest = case pat of
  M.CallFunc funcPat -> case (funcPat, cdest) of
    (FuncName name, Pil.CallFunc func) ->
      insist $ funcNameMatches name (func ^. #name)
            || maybeFuncNameMatches name (func ^? #symbol . #_Just . #_symbolName)
    (FuncName name, Pil.CallAddr (Pil.ConstFuncPtrOp _ mSym)) ->
      insist $ maybeFuncNameMatches name mSym
    (FuncName name, Pil.CallExtern extFunc) ->
      insist $ funcNameMatches name (extFunc ^. #name)

    (FuncNames names, Pil.CallFunc func) ->
      insist $ funcNameSetMatches names (func ^. #name)
            || maybeFuncNameSetMatches names (func ^? #symbol . #_Just . #_symbolName)
    (FuncNames names, Pil.CallAddr (Pil.ConstFuncPtrOp _ mSym)) ->
      insist $ maybeFuncNameSetMatches names mSym
    (FuncNames names, Pil.CallExtern extFunc) ->
      insist $ funcNameSetMatches names (extFunc ^. #name)
            || maybeFuncNameSetMatches names (extFunc ^? #symbol . #_Just . #_symbolName)
    (FuncAddr addr, Pil.CallFunc func) ->
      insist $ addr == func ^. #address
    (FuncAddr addr, Pil.CallAddr (Pil.ConstFuncPtrOp addr' _)) ->
      insist $ addr == addr'
    (FuncAddr _, Pil.CallExtern _) -> bad

    (FuncNameRegex rpat, Pil.CallFunc func) ->
      insist $ funcNameRegexMatches rpat (func ^. #name)
            || maybeFuncNameRegexMatches rpat (func ^? #symbol . #_Just . #_symbolName)
    (FuncNameRegex rpat, Pil.CallAddr (Pil.ConstFuncPtrOp _ mSym)) ->
      insist $ maybeFuncNameRegexMatches rpat mSym
    (FuncNameRegex rpat, Pil.CallExtern extFunc) ->
      insist $ funcNameRegexMatches rpat (extFunc ^. #name)
            || maybeFuncNameRegexMatches rpat (extFunc ^? #symbol . #_Just . #_symbolName)

    _ -> bad

  M.CallIndirect destPat -> case cdest of
    Pil.CallExpr destExpr -> matchExpr destPat destExpr
    _ -> bad


-- | performs action and returns result, but restores state to what it was
-- before the action
backtrack :: MatcherT expr stmt m a -> MatcherT expr stmt m a
backtrack action = do
  st <- get
  r <- action
  put st
  return r

-- | Runs the solver on the path up to this point. Includes remaining path
-- because the asserts might not be compatible with rest of path.
solvePath :: Monad m => MatcherT expr stmt m SolverResult
solvePath = do
  solver <- view #pathSolver
  ms <- get
  let stmts = reverse (ms ^. #parsedStmtsWithAssertions) <> ms ^. #remaining
  lift (solver stmts)

-- | Match via CallableWMI lookup (through call-sites) for a given PrimSpec.
-- Used by both CallsPrimitive (directly) and SubPrimitive (as first attempt).
matchCallableWMI
  :: forall stmt expr m.
     ( HasAddress stmt
     , IsExpression expr
     , IsStatement expr stmt
     , Monad m
     )
  => PrimSpec
  -> [(Symbol Pil.Expression, M.ExprPattern)]
  -> MatcherT expr stmt m ()
matchCallableWMI pt varPats = do
    stmt <- popStmt
    retireStmt stmt

    case mkCallStatement stmt of
      Nothing -> bad
      Just callStmt -> do
        let mdestFunc = case callStmt ^. #callOp . #dest of
              Pil.CallFunc ifunc -> Just $ BFunc.Internal ifunc
              Pil.CallExtern efunc -> Just $ BFunc.External efunc
              _ -> Nothing
        case mdestFunc of
          Nothing -> bad
          Just destFunc -> getCallableWMIs pt destFunc >>= \case
            Nothing -> bad
            Just wmis -> do
              let mRetExpr = maybe liftVar liftVarLike (callStmt ^. #callExpr)
                             <$> (callStmt ^. #resultVar) :: Maybe expr
                  argVector = V.fromList $ callStmt ^. #args
                  resolveFuncVar' :: FuncVarExpr -> Maybe expr
                  resolveFuncVar' = resolveFuncVar argVector mRetExpr
              let callablePats = toSnd mkStmtPatternFromCallableWMI <$> HashSet.toList wmis :: [(CallableWMI, M.Statement M.ExprPattern)]

              C.orr . flip fmap callablePats $ \(cprim, cpat) -> do
                matchStmt cpat stmt
                let mPrimVars = traverse (traverse (resolveFuncVar' . fst))
                      . HashMap.toList
                      $ cprim ^. #varMapping
                      :: Maybe [(Symbol Pil.Expression, expr)]
                case mPrimVars of
                  Nothing -> bad
                  Just primVars -> do
                    let varPatsMap = HashMap.fromList varPats
                    forM_ primVars $ \(varName, vexpr) -> case HashMap.lookup varName varPatsMap of
                      Nothing -> good
                      Just vpat -> matchExpr vpat vexpr

                    -- add constraints to stmts
                    forM_ (fmap fst $ cprim ^. #constraints :: [FuncVarExpr]) $ \fvExpr -> do
                      case resolveFuncVar' fvExpr of
                        Nothing -> bad
                        Just resolvedExpr -> do
                          #parsedStmtsWithAssertions %= ((mkStmtLike stmt $ Pil.Constraint (Pil.ConstraintOp resolvedExpr)) :)

-- | Match a SubPrimitive via inline pattern matching.
-- Runs the prim's stmtPattern with a fresh state (no variable bindings),
-- then restores the outer state except for consumed statements and
-- var bindings that are rebound through varPats.
-- The prim's pattern is expected to use CallsPrimitive for call-site matching.
matchSubPrimitive
  :: forall stmt expr m.
     ( HasAddress stmt
     , IsExpression expr
     , IsStatement expr stmt
     , Monad m
     )
  => M.Prim
  -> [(Symbol Pil.Expression, M.ExprPattern)]
  -> MatcherT expr stmt m ()
matchSubPrimitive prim varPats = do
    outerState <- get
    let freshState = (M.emptyMatcherState :: M.MatcherState expr stmt)
          & #remaining .~ (outerState ^. #remaining)
          & #callablePrimitives .~ (outerState ^. #callablePrimitives)
    -- Run the inline pattern in the fresh state
    put freshState
    matchPattern $ prim ^. #stmtPattern
    innerState <- get
    -- Restore outer state, but keep consumed stmts and parsed stmts from inline match
    put outerState
    #remaining .= (innerState ^. #remaining)
    #parsedStmtsWithAssertions .= (innerState ^. #parsedStmtsWithAssertions)
      <> (outerState ^. #parsedStmtsWithAssertions)
    -- Rebind vars from the inner match through varPats
    let innerBinds = innerState ^. #boundSyms
    forM_ varPats $ \(varName, vpat) -> do
      case HashMap.lookup varName innerBinds of
        Nothing -> bad
        Just vexpr -> matchExpr vpat vexpr
    -- Also propagate locations from the inner match
    #locations %= HashMap.union (innerState ^. #locations)

matchPattern
  :: forall stmt expr m.
     ( HasAddress stmt
     , IsExpression expr
     , IsStatement expr stmt
     , Monad m
     )
  => M.StmtPattern
  -> MatcherT expr stmt m ()
matchPattern = \case
  M.Stmt pat -> do
    stmt <- popStmt
    retireStmt stmt
    matchStmt pat stmt

  M.AvoidUntil x -> avoidUntil
    (const $ matchPattern $ x ^. #avoid)
    (matchPattern $ x ^. #until)

  M.Where subPat boundExprs -> do
    matchPattern subPat
    mLastParsedStmt <- headMay <$> use #parsedStmtsWithAssertions
    traverse_ (addConstraintLike mLastParsedStmt) boundExprs

  M.Necessarily subPat boundExprs -> do
    -- First check that negation of necessary constraints are UNSAT.
    matchPattern subPat
    mLastParsedStmt <- headMay <$> use #parsedStmtsWithAssertions
    resolvedExprs <- traverse resolveBoundExpr boundExprs
    let negatedExprs = fmap (absorbNots . negate) resolvedExprs
        mkOr x y = mkExprLike x . Pil.OR $ Pil.OrOp x y
        mNegatedDisjunction = case negatedExprs of
          [] -> Nothing
          (x:xs) -> Just $ foldl' mkOr x xs
        addConstraintExpr x = do
          let stmt = maybe mkDefStmt mkStmtLike mLastParsedStmt
                . Pil.Constraint
                . Pil.ConstraintOp
                $ x
          #parsedStmtsWithAssertions %= (stmt :)

    case mNegatedDisjunction of
      Nothing ->
        -- If there's no negated disjuction, it means there were no boundExprs
        -- and there's nothing to add or check beyond subPat, which has already
        -- been checked
        good
      Just negatedDisjunction -> do
        -- First check that negation of necessary constraints are UNSAT.
        backtrack (addConstraintExpr negatedDisjunction >> solvePath) >>= \case
          Unsat _ -> do
            -- Now add contraints to stmts, to be solved later
            traverse_ addConstraintExpr resolvedExprs
          _ -> bad

  M.EndOfPath -> use #remaining >>= \case
    [] -> good
    _ -> bad

  M.Location lname pat -> do
    stmt <- peekStmt
    #locations %= HashMap.insert lname (Right $ getAddress stmt)
    matchPattern pat
      
  M.SubPrimitive prim varPats ->
    matchSubPrimitive prim varPats

  M.CallsPrimitive primSpec varPats ->
    matchCallableWMI primSpec varPats

-- TODO: remove. saving in case needed by References
--   M.Primitive pt varPats -> do
--     stmt <- popStmt
--     retireStmt stmt
--     -- for matching subpatterns, one approach
--     -- could be to make this not just check
--     -- if the callee has the WMI but also check
--     -- if the pattern for the primitive exists
--     -- (in the caller). this would change the
--     -- meaning of the Primitive StmtPattern,
--     -- but I think it would be an improvement.
--     -- this would allow us to eliminate special
--     -- treatment for StdLibPrimitives as well.
--     -- this would encourage (but not per se
--     -- require) changes to the matcher to
--     -- eliminate the need to start primitive
--     -- patterns with Star. I believe this would
--     -- be an improvement
--     case mkCallStatement stmt of
--       Nothing -> bad
--       Just callStmt -> do
--         let mdestFunc = case callStmt ^. #callOp . #dest of
--               Pil.CallFunc ifunc -> Just $ BFunc.Internal ifunc
--               Pil.CallExtern efunc -> Just $ BFunc.External efunc
--               _ -> Nothing
--         case mdestFunc of
--           Nothing -> bad
--           Just destFunc -> getCallableWMIs pt destFunc >>= \case
--             Nothing -> bad
--             Just wmis -> do
--               let mRetExpr = maybe liftVar liftVarLike (callStmt ^. #callExpr)
--                              <$> (callStmt ^. #resultVar) :: Maybe expr
--                   argVector = V.fromList $ callStmt ^. #args
--                   resolveFuncVar' :: FuncVarExpr -> Maybe expr
--                   resolveFuncVar' = resolveFuncVar argVector mRetExpr
--                   -- TODO: We are recreating these every single stmt we check for a Primitive.
--                   --       Instead, we could cache them in the MatcherState
--               let callablePats = toSnd mkStmtPatternFromCallableWMI <$> HashSet.toList wmis :: [(CallableWMI, M.Statement M.ExprPattern)]

--               C.orr . flip fmap callablePats $ \(cprim, cpat) -> do
--                 matchStmt cpat stmt
--                 let mPrimVars = traverse (traverse (resolveFuncVar' . fst))
--                       . HashMap.toList
--                       $ cprim ^. #varMapping
--                       :: Maybe [(Symbol Pil.Expression, expr)]
--                 case mPrimVars of
--                   Nothing -> bad
--                   Just primVars -> do
--                     forM_ primVars $ \(varName, vexpr) -> case HashMap.lookup varName varPats of
--                       Nothing -> good
--                       Just vpat -> matchExpr vpat vexpr

--                     -- add constraints to stmts
--                     forM_ (fmap fst $ cprim ^. #constraints :: [FuncVarExpr]) $ \fvExpr -> do
--                       case resolveFuncVar' fvExpr of
--                         Nothing -> bad
--                         Just resolvedExpr -> do
--                           #parsedStmtsWithAssertions %= ((mkStmtLike stmt $ Pil.Constraint (Pil.ConstraintOp resolvedExpr)) :)

  M.Star -> use #remaining >>= \case
    [] -> good
    _ -> good <|> (popAndRetireStmt >> matchPattern M.Star)

  M.And a b -> do
    matchPattern a
    matchPattern b

  M.Or a b -> matchPattern a <|||> matchPattern b

  M.Good -> good

  M.Bad -> bad

resolveFuncVar
  :: IsExpression expr
  => Vector expr
  -> Maybe expr
  -> FuncVarExpr
  -> Maybe expr
resolveFuncVar argExprs mRetExpr = \case
  (Prim.FuncVarExpr sz op) -> do
    sz' <- case sz of
      Prim.ConstSize n -> return n
      Prim.SizeOf fv -> case fv of
        Prim.Ret -> getExprSize <$> mRetExpr
        Prim.Arg n -> getExprSize <$> argExprs ^? ix (fromIntegral n)
        Prim.Global _ -> Nothing
    fmap (mkExprWithSize sz')
      . traverse (resolveFuncVar argExprs mRetExpr)
      $ op
 
  (Prim.FuncVar fv) -> case fv of
    Prim.Ret -> mRetExpr
    (Prim.Global x) -> Just $ liftExpression x
    (Prim.Arg n) -> argExprs ^? ix (fromIntegral n)
  where
    liftExpression :: IsExpression expr => Pil.Expression -> expr
    liftExpression (Pil.Expression sz op) = mkExprWithSize sz (fmap liftExpression op)

mkStmtPatternFromCallableWMI
  :: CallableWMI
  -> M.Statement M.ExprPattern
mkStmtPatternFromCallableWMI x =
  M.Call retPattern (M.CallFunc $ x ^. #callDest) []
  where
    retPattern = case HashSet.member Prim.Ret $ x ^. #linkedVars of
      False -> Nothing
      True -> Just M.Wild

-- | Puts statement onto the parsed statements list
retireStmt :: stmt -> MatcherT expr stmt m ()
retireStmt stmt = #parsedStmtsWithAssertions %= (stmt:)

popStmt :: Monad m => MatcherT expr stmt m stmt
popStmt = use #remaining >>= \case
  [] -> bad
  (x:xs) -> do
    #remaining .= xs
    return x

peekStmt :: Monad m => MatcherT expr stmt m stmt
peekStmt = use #remaining >>= \case
  [] -> bad
  (x:_) -> return x

peekStmt' :: MatcherT expr stmt m (Maybe stmt)
peekStmt' = use #remaining >>= \case
  [] -> return Nothing
  (x:_) -> return $ Just x

popAndRetireStmt :: Monad m => MatcherT expr stmt m ()
popAndRetireStmt = popStmt >>= retireStmt

-- | Tries to match a series of statements with a list of patterns.
-- Returns MatcherStates of all successful matches of pattern in path
runMatchStmts
  :: forall m expr stmt.
     ( Monad m
     , HasAddress stmt
     , IsStatement expr stmt
     , IsExpression expr
     )
  => Int
  -> MatcherCtx stmt m
  -> MatcherState expr stmt
  -> M.StmtPattern
  -> m [MatcherState expr stmt]
runMatchStmts maxResults ctx st pat = fmap (fmap snd). observeManyMatcherT ctx st maxResults $ do
  matchPattern pat
  drainRemainingStmts
  where
    drainRemainingStmts :: MatcherT expr stmt m ()
    drainRemainingStmts = do
      remaining <- use #remaining
      #parsedStmtsWithAssertions %= (reverse remaining <>)

mkMatcherState
  :: StmtSolver stmt m
  -> PathPrep stmt
  -> (MatcherCtx stmt m, MatcherState expr stmt)
mkMatcherState solver pathPrep =
  ( MatcherCtx solver (pathPrep ^. #taintSet) (pathPrep ^. #taintPropagators)
  , MatcherState (pathPrep ^. #stmts) HashMap.empty HashMap.empty HashMap.empty [] Nothing HashMap.empty HashMap.empty
  )

newtype ResolveBoundExprError = CannotFindBoundVarInState (Symbol Pil.Expression)
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)

lookupBound :: Monad m => Symbol Pil.Expression -> MatcherT expr stmt m expr
lookupBound sym = use #boundSyms >>= maybe bad return . HashMap.lookup sym

resolveBoundExprSize
  :: ( Monad m
     , IsExpression expr
     )
  => BoundExprSize
  -> MatcherT expr stmt m (Size Pil.Expression)
resolveBoundExprSize (ConstSize sz) = return sz
resolveBoundExprSize (SizeOf sym) = getExprSize <$> lookupBound sym

resolveBoundExpr
  :: (IsExpression expr, Monad m)
  => BoundExpr
  -> MatcherT expr stmt m expr
resolveBoundExpr (BoundRef sym) = lookupBound sym
resolveBoundExpr (BoundExpr bsize op) = do
  mkExprWithSize <$> resolveBoundExprSize bsize <*> traverse resolveBoundExpr op


addConstraintLike
  :: ( IsStatement expr stmt
     , IsExpression expr
     , Monad m
     )
  => Maybe stmt
  -> BoundExpr
  -> MatcherT expr stmt m ()
addConstraintLike mstmt x = do
  x' <- resolveBoundExpr x
  let stmt = maybe mkDefStmt mkStmtLike mstmt . Pil.Constraint . Pil.ConstraintOp $ x'
  #parsedStmtsWithAssertions %= (stmt :)

resolveBoundText
  :: IsExpression expr
  => HashMap (Symbol Pil.Expression) expr
  -> BoundText
  -> Text
resolveBoundText m (TextExpr sym) = maybe ("<cannot find expr sym: " <> cs sym <> ">") (pretty' . asExpression)
  $ HashMap.lookup sym m
resolveBoundText _ (PureText t) = t
resolveBoundText m (CombineText a b) = resolveBoundText m a <> resolveBoundText m b
resolveBoundText m (CaseContains bt cases) = let t = resolveBoundText m bt in
  maybe ("<" <> t <> " matches no cases: " <> show (fst <$> cases) <> ">")
  (resolveBoundText m)
  . headMay
  . mapMaybe (\(c, r) -> if Text.isInfixOf c t then Just r else Nothing)
  $ cases


-- | Matches list of statements with pattern. Returns new list of statements
-- that may include added assertions.
-- It runs the solver and rejects matches that don't solve.
match_
  :: ( Monad m
     , HasAddress stmt
     , IsStatement expr stmt
     , IsExpression expr
     )
  => Int
  -> MatcherCtx stmt m
  -> MatcherState expr stmt
  -> M.StmtPattern
  -> m [(MatcherState expr stmt, [stmt])]
match_ maxResults mctx mstate pats = do
  runMatchStmts maxResults mctx mstate pats >>= mapMaybeM solveAndFinalize
  where
    solveAndFinalize ms = do
      let parsed = reverse $ ms ^. #parsedStmtsWithAssertions
      (mctx ^. #pathSolver $ parsed) >>= \case
        Sat r -> return $ Just (ms & #solutions ?~ r, parsed)
        _ -> return Nothing

-- | Matches list of statements with pattern. Returns new list of statements
-- that may include added assertions.
match
  :: ( Monad m
     , HasAddress stmt
     , IsStatement expr stmt
     , IsExpression expr
     )
  => Int
  -> StmtSolver stmt m
  -> M.StmtPattern
  -> PathPrep stmt
  -> m [(MatcherState expr stmt, [stmt])]
match maxResults solver pats pathPrep = match_ maxResults mctx mstate pats
  where
    (mctx, mstate) = mkMatcherState solver pathPrep

singleMatch
  :: ( Monad m
     , HasAddress stmt
     , IsStatement expr stmt
     , IsExpression expr
     )
  => StmtSolver stmt m
  -> M.StmtPattern
  -> PathPrep stmt
  -> m (Maybe (MatcherState expr stmt, [stmt]))
singleMatch solver pats = fmap headMay . match 1 solver pats

singlePureMatch
  :: ( HasAddress stmt
     , IsStatement expr stmt
     , IsExpression expr
     )
  => M.StmtPattern
  -> PathPrep stmt
  -> Maybe (MatcherState expr stmt, [stmt])
singlePureMatch pats = runIdentity . fmap headMay . match 1 dummySolver pats

singleMatch_
  :: ( Monad m
     , HasAddress stmt
     , IsStatement expr stmt
     , IsExpression expr
     )
  => MatcherCtx stmt m
  -> MatcherState expr stmt
  -> M.StmtPattern
  -> m (Maybe (MatcherState expr stmt, [stmt]))
singleMatch_ mctx mstate = fmap headMay . match_ 1 mctx mstate
