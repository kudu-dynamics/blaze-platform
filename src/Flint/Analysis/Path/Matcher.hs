module Flint.Analysis.Path.Matcher
 ( module Flint.Analysis.Path.Matcher
 ) where

import Flint.Prelude hiding (Symbol, sym)
import Flint.Analysis.Uefi ( resolveCalls )

import Blaze.Cfg.Path (PilPath)
import qualified Blaze.Cfg.Path as Path
import qualified Blaze.Pil.Analysis.Path as PA
import Blaze.Pretty (pretty')
import qualified Blaze.Types.Pil as Pil
import qualified Blaze.Types.Function as BFunc
import Blaze.Types.Pil (Size)

import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Data.String (IsString(fromString))
import qualified Data.Text as Text


type Symbol = Text

data Func
  = FuncName Text
  | FuncAddr Address
  deriving (Eq, Ord, Show, Hashable, Generic)

data CallDest expr
  = CallFunc Func
  | CallIndirect expr
  deriving (Eq, Ord, Show, Hashable, Generic)

data Statement expr
  = Def expr expr -- Def dst src; dst is always (Var PilVar) expr
  | Constraint expr
  | Store expr expr
  | EnterFunc Func -- need one that lets you access args of expanded call?
  | Call (Maybe expr) (CallDest expr) [expr]
  | BranchCond expr
  | Jump expr
  | Ret expr
  | NoRet
  -- These are sort of supported by Call, since it matches against any CallStatement
  -- | TailCall (CallDest expr) [expr]
  deriving (Eq, Ord, Show, Hashable, Generic)

data StmtPattern
  = Stmt (Statement ExprPattern)
  -- | Eventually StmtPattern
  -- | Avoid StmtPattern
  | AvoidUntil AvoidSpec
  | AnyOne [StmtPattern]  -- One match succeeds. [] immediately succeeeds.
  | Unordered [StmtPattern] -- matches all, but in no particular order.
  | Ordered [StmtPattern] -- matches all. Good for grouping and scoping Where bounds.
  -- | Add a constraint that 'src' is involved in the definition of 'dst'
  | Taints {src :: BoundExpr, dst :: BoundExpr}
  | Assert BoundExpr -- Add a boolean expr constraint, using bound variables.
  deriving (Eq, Ord, Show, Hashable, Generic)

data BoundExprSize
  = ConstSize (Size Pil.Expression)
  | SizeOf Symbol  -- looks up symbol to get size of expr
  deriving (Eq, Ord, Show, Hashable, Generic)

data BoundExpr
  = Bound Symbol -- gets expression that has been bound with Bind
  | BoundExpr BoundExprSize (Pil.ExprOp BoundExpr)
  deriving (Eq, Ord, Show, Hashable, Generic)

-- | Text that can refer to variables bound during pattern matching
data BoundText
  = TextExpr Symbol
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
  | Bind Symbol ExprPattern

  -- | Matches prefix of var name, like "arg4" will match "arg4-7#1".
  -- Also matches against ConstFuncPtrs that a name.
  | Var Symbol

  -- | Matches if its an immediate, like a const int, ptr, float, etc.
  | Immediate
  
  -- | Matches if ExprPattern matches somewhere inside expr
  | Contains ExprPattern
  | Wild

  -- | Inequalities. These match on converse inequalities that mean the same thing,
  -- like `not (x != y)` would match `x .== y`, and `x < y` would match `y .> x`
  -- It also will work for signed or unsigned ints and floats.
  -- TODO: add explicit signed/unsigned variants if deemed important
  -- TODO: do we need NOT? and if so, do we want a thing that just says
  --       the expr needs to be true?
  | Cmp CmpType ExprPattern ExprPattern
  deriving (Eq, Ord, Show, Hashable, Generic)

data CmpType
  = CmpE
  | CmpNE
  | CmpGT
  | CmpGE
  | CmpLT
  | CmpLE
  deriving (Eq, Ord, Show, Hashable, Generic)

(.==) :: ExprPattern -> ExprPattern -> ExprPattern
(.==) a b = Cmp CmpE a b
infix 4 .==

(./=) :: ExprPattern -> ExprPattern -> ExprPattern
(./=) a b = Cmp CmpNE a b
infix 4 ./=

(.<) :: ExprPattern -> ExprPattern -> ExprPattern
(.<) a b = Cmp CmpLT a b
infix 4 .<

(.<=) :: ExprPattern -> ExprPattern -> ExprPattern
(.<=) a b = Cmp CmpLE a b
infix 4 .<=

(.>) :: ExprPattern -> ExprPattern -> ExprPattern
(.>) a b = Cmp CmpGT a b
infix 4 .>

(.>=) :: ExprPattern -> ExprPattern -> ExprPattern
(.>=) a b = Cmp CmpGE a b
infix 4 .>=

data AvoidSpec = AvoidSpec
  { avoid :: StmtPattern
  , until :: Maybe StmtPattern
  } deriving (Eq, Ord, Show, Hashable, Generic)

data MatcherState = MatcherState
  { remainingStmts :: [Pil.Stmt]
  , boundSyms :: HashMap Symbol Pil.Expression
  , avoids :: HashSet AvoidSpec
  -- The successfully parsed stmts, stored in reverse order
  -- possibly interleaved with Assertions
  , parsedStmtsWithAssertions :: [Either BoundExpr Pil.Stmt]
  } deriving Generic

newtype Matcher a = Matcher {
  _runMatcher :: ExceptT () (StateT MatcherState Identity) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadError ()
    , MonadState MatcherState
    , Alternative
    )

runMatcher_ :: Matcher a -> MatcherState ->  (Either () a, MatcherState)
runMatcher_ action s
  = runIdentity
  . flip runStateT s
  . runExceptT
  . _runMatcher
  $ action

mkMatcherState :: [Pil.Stmt] -> MatcherState
mkMatcherState stmts = MatcherState stmts HashMap.empty HashSet.empty []

runMatcher :: [Pil.Stmt] -> Matcher a -> (MatcherState, Maybe a)
runMatcher stmts action = case runMatcher_ action (mkMatcherState stmts) of
  (Left _, s) -> (s, Nothing)
  (Right x, s) -> (s, Just x)

-- | Gets and removes the next statement from remainingStatements
takeNextStmt :: Matcher (Maybe Pil.Stmt)
takeNextStmt = use #remainingStmts >>= \case
  [] -> return Nothing
  (x:xs) -> do
    #remainingStmts .= xs
    return $ Just x

retireStmt :: Matcher ()
retireStmt = takeNextStmt >>= \case
  Nothing -> return ()
  Just x -> #parsedStmtsWithAssertions %= (Right x :)

-- | Gets the next statement from remainingStatements
peekNextStmt :: Matcher (Maybe Pil.Stmt)
peekNextStmt = use #remainingStmts >>= \case
  [] -> return Nothing
  (x:_) -> return $ Just x

-- | Restores a statement to the beginning of remainingStatements
restoreStmt :: Pil.Stmt -> Matcher ()
restoreStmt stmt = #remainingStmts %= (stmt:)

matchCallDest :: CallDest ExprPattern -> Pil.CallDest Pil.Expression -> Matcher ()
matchCallDest pat cdest = case pat of
  CallFunc funcPat -> case (funcPat, cdest) of
    (FuncName name, Pil.CallFunc func) ->
      insist $ func ^. #name == name
            || func ^? #symbol . #_Just . #_symbolName == Just name
    (FuncName name, Pil.CallAddr (Pil.ConstFuncPtrOp _ mSym)) ->
      insist $ Just name == mSym
    (FuncName name, Pil.CallExtern (Pil.ExternPtrOp _addr _off mSym)) ->
      insist $ Just name == mSym

    (FuncAddr addr, Pil.CallFunc func) ->
      insist $ addr == func ^. #address
    (FuncAddr addr, Pil.CallAddr (Pil.ConstFuncPtrOp addr' _)) ->
      insist $ addr == addr'
    (FuncAddr addr, Pil.CallExtern (Pil.ExternPtrOp addr' _off _mSym)) ->
      insist $ addr == addr'
    _ -> bad
  CallIndirect destPat -> case cdest of
    Pil.CallExpr destExpr -> matchExpr destPat destExpr
    _ -> bad

good :: Matcher ()
good = return ()

bad :: Matcher ()
bad = throwError ()

insist :: Bool -> Matcher ()
insist = bool bad good

-- | This either adds a new sym/expr combo to the var bindings,
-- or if the sym already exists, it checks to see if it matches.
bind :: Symbol -> Pil.Expression -> Matcher ()
bind sym expr = do
  bsyms <- use #boundSyms
  case HashMap.lookup sym bsyms of
    Just expr' -> insist $ expr == expr'
    Nothing -> #boundSyms %= HashMap.insert sym expr


-- | Tries to absorb a "not" into a bool expression.
-- For instance, `(not (x == y))` will become `(x != y)`,
-- `(not (x < y))` will become `(x >= y)`, and `(not (not x))`
-- will become `x`, and `(not x)` will just remain `(not x)`
-- if Bool is true, then try to negate the expr
absorbNots_ :: Bool -> Pil.Expression -> Pil.Expression
-- The True case means an outer "Not" has already been foudn
absorbNots_ True expr = case expr ^. #op of
  -- (not (not x)) = x
  Pil.NOT (Pil.NotOp x) -> absorbNots_ False x

  Pil.CMP_E (Pil.CmpEOp a b) -> mkExpr . Pil.CMP_NE $ Pil.CmpNeOp a b
  Pil.CMP_NE (Pil.CmpNeOp a b) -> mkExpr . Pil.CMP_E $ Pil.CmpEOp a b

  Pil.CMP_SGE (Pil.CmpSgeOp a b) -> mkExpr . Pil.CMP_SLT $ Pil.CmpSltOp a b
  Pil.CMP_SGT (Pil.CmpSgtOp a b) -> mkExpr . Pil.CMP_SLE $ Pil.CmpSleOp a b
  Pil.CMP_SLE (Pil.CmpSleOp a b) -> mkExpr . Pil.CMP_SGT $ Pil.CmpSgtOp a b
  Pil.CMP_SLT (Pil.CmpSltOp a b) -> mkExpr . Pil.CMP_SGE $ Pil.CmpSgeOp a b

  Pil.CMP_UGE (Pil.CmpUgeOp a b) -> mkExpr . Pil.CMP_ULT $ Pil.CmpUltOp a b
  Pil.CMP_UGT (Pil.CmpUgtOp a b) -> mkExpr . Pil.CMP_ULE $ Pil.CmpUleOp a b
  Pil.CMP_ULE (Pil.CmpUleOp a b) -> mkExpr . Pil.CMP_UGT $ Pil.CmpUgtOp a b
  Pil.CMP_ULT (Pil.CmpUltOp a b) -> mkExpr . Pil.CMP_UGE $ Pil.CmpUgeOp a b

  Pil.FCMP_E (Pil.FcmpEOp a b) -> mkExpr . Pil.FCMP_NE $ Pil.FcmpNeOp a b
  Pil.FCMP_NE (Pil.FcmpNeOp a b) -> mkExpr . Pil.FCMP_E $ Pil.FcmpEOp a b

  Pil.FCMP_GE (Pil.FcmpGeOp a b) -> mkExpr . Pil.FCMP_LT $ Pil.FcmpLtOp a b
  Pil.FCMP_GT (Pil.FcmpGtOp a b) -> mkExpr . Pil.FCMP_LE $ Pil.FcmpLeOp a b
  Pil.FCMP_LE (Pil.FcmpLeOp a b) -> mkExpr . Pil.FCMP_GT $ Pil.FcmpGtOp a b
  Pil.FCMP_LT (Pil.FcmpLtOp a b) -> mkExpr . Pil.FCMP_GE $ Pil.FcmpGeOp a b

  -- Put the NOT back on because it can't be absorbed
  _ -> mkExpr . Pil.NOT $ Pil.NotOp expr

  where mkExpr = Pil.Expression $ expr ^. #size
absorbNots_ False expr = case expr ^. #op of
  Pil.NOT (Pil.NotOp x) -> absorbNots_ True x
  _ -> expr

absorbNots :: Pil.Expression -> Pil.Expression
absorbNots = absorbNots_ False

matchCmp
  :: CmpType
  -> ExprPattern
  -> ExprPattern
  -> Pil.Expression
  -> Matcher ()
matchCmp cmpType patA patB expr = case cmpType of
  CmpE -> case op of
    Pil.CMP_E (Pil.CmpEOp a b) -> bimatch a b <|> bimatch b a
    Pil.FCMP_E (Pil.FcmpEOp a b) -> bimatch a b <|> bimatch b a
    _ -> bad
  CmpNE -> case op of
    Pil.CMP_NE (Pil.CmpNeOp a b) -> bimatch a b <|> bimatch b a
    Pil.FCMP_NE (Pil.FcmpNeOp a b) -> bimatch a b <|> bimatch b a
    _ -> bad

  CmpGT -> case op of
    Pil.CMP_SGT (Pil.CmpSgtOp a b) -> bimatch a b
    Pil.CMP_UGT (Pil.CmpUgtOp a b) -> bimatch a b
    Pil.CMP_SLT (Pil.CmpSltOp a b) -> bimatch b a
    Pil.CMP_ULT (Pil.CmpUltOp a b) -> bimatch b a

    Pil.FCMP_GT (Pil.FcmpGtOp a b) -> bimatch a b
    Pil.FCMP_LT (Pil.FcmpLtOp a b) -> bimatch b a
    _ -> bad

  CmpGE -> case op of
    Pil.CMP_SGE (Pil.CmpSgeOp a b) -> bimatch a b
    Pil.CMP_UGE (Pil.CmpUgeOp a b) -> bimatch a b
    Pil.CMP_SLE (Pil.CmpSleOp a b) -> bimatch b a
    Pil.CMP_ULE (Pil.CmpUleOp a b) -> bimatch b a

    Pil.FCMP_GE (Pil.FcmpGeOp a b) -> bimatch a b
    Pil.FCMP_LE (Pil.FcmpLeOp a b) -> bimatch b a
    _ -> bad

  CmpLT -> case op of
    Pil.CMP_SLT (Pil.CmpSltOp a b) -> bimatch a b
    Pil.CMP_ULT (Pil.CmpUltOp a b) -> bimatch a b
    Pil.CMP_SGT (Pil.CmpSgtOp a b) -> bimatch b a
    Pil.CMP_UGT (Pil.CmpUgtOp a b) -> bimatch b a

    Pil.FCMP_LT (Pil.FcmpLtOp a b) -> bimatch a b
    Pil.FCMP_GT (Pil.FcmpGtOp a b) -> bimatch b a
    _ -> bad

  CmpLE -> case op of
    Pil.CMP_SLE (Pil.CmpSleOp a b) -> bimatch a b
    Pil.CMP_ULE (Pil.CmpUleOp a b) -> bimatch a b
    Pil.CMP_SGE (Pil.CmpSgeOp a b) -> bimatch b a
    Pil.CMP_UGE (Pil.CmpUgeOp a b) -> bimatch b a

    Pil.FCMP_LE (Pil.FcmpLeOp a b) -> bimatch a b
    Pil.FCMP_GE (Pil.FcmpGeOp a b) -> bimatch b a
    _ -> bad

  where
    bimatch a b = backtrackOnError $ matchExpr patA a >> matchExpr patB b
    exprWithAbsorbedNots = absorbNots expr
    op = exprWithAbsorbedNots ^. #op

matchExprOp :: Pil.ExprOp ExprPattern -> Pil.ExprOp Pil.Expression -> Matcher ()
matchExprOp opPat op = do
  insist $ (const () <$> opPat) == (const () <$> op)
  traverse_ (uncurry matchExpr) $ zip (toList opPat) (toList op)  

matchExpr :: ExprPattern -> Pil.Expression -> Matcher ()
matchExpr pat expr = case pat of
  Bind sym xpat -> do
    matchExpr xpat expr
    -- success
    bind sym expr
  Var prefixOfName -> case expr ^. #op of
    Pil.VAR (Pil.VarOp pv) -> insist . Text.isPrefixOf prefixOfName $ pv ^. #symbol
    Pil.ConstFuncPtr (Pil.ConstFuncPtrOp _addr (Just symb)) -> do
      insist $ Text.isPrefixOf prefixOfName symb
    _ -> bad
  Immediate -> case expr ^. #op of
    Pil.CONST _ -> good
    Pil.CONST_PTR _ -> good
    Pil.CONST_FLOAT _ -> good
    Pil.ConstStr _ -> good
    Pil.ConstFuncPtr _ -> good
    _ -> bad
  Contains xpat -> do
    backtrackOnError (matchExpr xpat expr)
      <|> asum (backtrackOnError . matchExpr (Contains xpat) <$> toList (expr ^. #op))
  Wild -> return ()
  Expr xop -> matchExprOp xop $ expr ^. #op
  Cmp cmpType patA patB -> matchCmp cmpType patA patB expr

matchFuncPatWithFunc :: Func -> BFunc.Function -> Matcher ()
matchFuncPatWithFunc (FuncName name) func = insist
  $ func ^. #name == name
  || func ^? #symbol . #_Just . #_symbolName == Just name
matchFuncPatWithFunc (FuncAddr addr) func = insist $ addr == func ^. #address


matchStmt :: Statement ExprPattern -> Pil.Stmt -> Matcher ()
matchStmt sPat stmt = case (sPat, stmt) of
  (Def destPat srcPat, Pil.Def (Pil.DefOp pv expr)) -> do
    let pvExpr = Pil.Expression (expr ^. #size) . Pil.VAR $ Pil.VarOp pv
    matchExpr destPat pvExpr
    matchExpr srcPat expr
  (Constraint expr, Pil.Constraint (Pil.ConstraintOp condExpr)) -> do
    matchExpr expr condExpr
  (Store addrPat valPat, Pil.Store (Pil.StoreOp addrExpr valExpr)) -> do
    matchExpr addrPat addrExpr
    matchExpr valPat valExpr
  (EnterFunc funcPat, Pil.EnterContext (Pil.EnterContextOp ctx)) ->
    matchFuncPatWithFunc funcPat $ ctx ^. #func
  (Call mResultPat callDestPat argPats, _) -> case Pil.mkCallStatement stmt of
    Nothing -> bad
    Just (Pil.CallStatement _ callOp argExprs mResultVar) -> do
      matchCallDest callDestPat $ callOp ^. #dest
      case (mResultPat, mResultVar) of
        (Nothing, _) -> good
        (Just resultPat, Just resultVar) -> do
          let pvExpr = Pil.Expression (coerce $ resultVar ^. #size) . Pil.VAR $ Pil.VarOp resultVar
          matchExpr resultPat pvExpr
        _ -> bad
      -- It's ok if there are less arg pats than there are args
      -- I don't think we should make them match, since the lifter gets
      -- the number of args wrong sometimes
      traverse_ (uncurry matchExpr) $ zip argPats argExprs
  (BranchCond condPat, Pil.BranchCond (Pil.BranchCondOp condExpr)) ->
    matchExpr condPat condExpr
  (Jump destPat, Pil.Jump (Pil.JumpOp destExpr)) ->
    matchExpr destPat destExpr
  (Ret valPat, Pil.Ret (Pil.RetOp valExpr)) ->
    matchExpr valPat valExpr
  (NoRet, Pil.NoRet) -> good
  _ -> bad

backtrackOnError :: Matcher a -> Matcher a
backtrackOnError action = do
  s <- get
  action <|> (put s >> throwError ())

-- | Runs action and always backtracks state
backtrack :: Matcher a -> Matcher a
backtrack action = do
  s <- get
  tryError action >>= \case
    Left _ -> put s >> throwError ()
    Right x -> put s >> return x

addBoundExpr :: BoundExpr -> Matcher ()
addBoundExpr x = #parsedStmtsWithAssertions %= (Left x :)

storeAsParsed :: Pil.Stmt -> Matcher ()
storeAsParsed x = #parsedStmtsWithAssertions %= (Right x :)

checkAvoid :: AvoidSpec -> Matcher ()
checkAvoid fullAvoid@(AvoidSpec avoid' mUntil) = case mUntil of
  Nothing -> tryAvoid
  Just until' -> tryError (backtrack $ matchNextStmt_ False False until') >>= \case
    Left _ -> tryAvoid
    -- Matched an "until", so remove the Avoid from the avoid state.
    Right _ -> #avoids %= HashSet.delete fullAvoid
  where
    tryAvoid = tryError (backtrack $ matchNextStmt_ False False avoid') >>= \case
      Left _ -> good -- Avoid has been avoided 
      Right _ -> bad

-- | This checks all the avoids and fails if any occur.
-- The only state this changes is the avoid list
checkAvoids :: Matcher ()
checkAvoids = use #avoids >>= traverse_ checkAvoid

matchNextStmt :: StmtPattern -> Matcher ()
matchNextStmt = matchNextStmt_ True True

-- | Matches the next statement with the next stmt pattern.
matchNextStmt_ :: Bool -> Bool -> StmtPattern -> Matcher ()
matchNextStmt_ firstCheckAvoids tryNextStmtOnFailure pat = when firstCheckAvoids checkAvoids >> peekNextStmt >>= \case
  Nothing -> case pat of
    Stmt _ -> bad
    AvoidUntil _ -> good
    AnyOne [] -> good
    AnyOne _ -> bad
    Unordered [] -> good
    Unordered _ -> bad
    Ordered [] -> good
    Ordered _ -> bad
    Taints _ _ -> good  -- FIXME
    Assert boundExpr -> addBoundExpr boundExpr >> good
  Just stmt -> case pat of
    Stmt sPat -> tryError (matchStmt sPat stmt) >>= \case
      -- Matched Statement
      Right _ -> retireStmt
      -- Stmt failed to match. Try next stmt with same pattern.
      Left _ -> perhapsRecur
    AvoidUntil avoidSpec -> #avoids %= HashSet.insert avoidSpec
    AnyOne [] -> return ()
    AnyOne pats -> do
      tryError (asum $ backtrackOnError . matchNextStmt <$> pats) >>= \case
        -- One of them matched.
        Right _ -> return ()
        -- Nothing matched. Try next stmt with same pattern.
        Left _ -> perhapsRecur
    Unordered [] -> return ()
    Unordered pats -> do
      tryError (asum
                 $ backtrackOnError . traverse (matchNextStmt_ True False) <$> zip [0..] pats
               ) >>= \case
        -- One matched, now remove successful pattern from pats and continue
        Right (i, _) -> do
          matchNextStmt . Unordered $ removeNth i pats
        -- No matches. Continue to next statement with same pattern.
        Left _ -> perhapsRecur
    Ordered [] -> return ()
    Ordered (p:pats) -> tryError (backtrackOnError $ matchNextStmt p) >>= \case
      Right _ -> matchNextStmt $ Ordered pats
      Left _ -> perhapsRecur
    Taints _ _ -> pure ()  -- FIXME
    Assert bexpr -> addBoundExpr bexpr
  where
    perhapsRecur = if tryNextStmtOnFailure
      then do
        retireStmt
        matchNextStmt pat
      else throwError ()

newtype ResolveBoundExprError = CannotFindBoundVarInState Symbol
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)

resolveBoundExprSize
  :: HashMap Symbol Pil.Expression
  -> BoundExprSize
  -> Either ResolveBoundExprError (Size Pil.Expression)
resolveBoundExprSize _ (ConstSize sz) = Right sz
resolveBoundExprSize m (SizeOf sym) = case HashMap.lookup sym m of
  Nothing -> Left $ CannotFindBoundVarInState sym
  Just expr -> Right $ expr ^. #size

resolveBoundExpr
  :: HashMap Symbol Pil.Expression
  -> BoundExpr
  -> Either ResolveBoundExprError Pil.Expression
resolveBoundExpr m (Bound sym) =
  maybe (Left $ CannotFindBoundVarInState sym) Right $ HashMap.lookup sym m
resolveBoundExpr m (BoundExpr bsize op) =
  Pil.Expression <$> resolveBoundExprSize m bsize <*> traverse (resolveBoundExpr m) op

resolveBoundText
  :: HashMap Symbol Pil.Expression
  -> BoundText
  -> Text
resolveBoundText m (TextExpr sym) = maybe ("<cannot find expr sym: " <> sym <> ">") pretty'
  $ HashMap.lookup sym m
resolveBoundText _ (PureText t) = t
resolveBoundText m (CombineText a b) = resolveBoundText m a <> resolveBoundText m b
resolveBoundText m (CaseContains bt cases) = let t = resolveBoundText m bt in
  maybe ("<" <> t <> " matches no cases: " <> show (fst <$> cases) <> ">")
  (resolveBoundText m)
  . headMay
  . mapMaybe (\(c, r) -> if Text.isInfixOf c t then Just r else Nothing)
  $ cases

-- | Returns resolved statements as well as the count of assertions.
getStmtsWithResolvedBounds :: MatcherState -> Either ResolveBoundExprError (Int, [Pil.Stmt])
getStmtsWithResolvedBounds s = foldM f (0, []) $ s ^. #parsedStmtsWithAssertions
  where
    f :: (Int, [Pil.Stmt])
      -> Either BoundExpr Pil.Stmt
      -> Either ResolveBoundExprError (Int, [Pil.Stmt])
    f (assertionCount, xs) (Right stmt) = return (assertionCount, stmt:xs)
    f (assertionCount, xs) (Left bexpr) = do
      expr <- resolveBoundExpr (s ^. #boundSyms) bexpr
      let stmt = Pil.Constraint (Pil.ConstraintOp expr)
      return (assertionCount + 1, stmt:xs)

-- | Tries to match a series of statements with a list of patterns.
-- Returns MatcherState and bool indicating initial success.
-- Any Asserts that were generating during the match will need to be
-- sent to the solver later.
runMatchStmts :: [StmtPattern] -> [Pil.Stmt] -> (MatcherState, Bool)
runMatchStmts pats stmts = over _2 (maybe False (const True)) . runMatcher stmts $ do
  traverse_ matchNextStmt pats
  drainRemainingStmts
  where
    drainRemainingStmts :: Matcher ()
    drainRemainingStmts = use #remainingStmts >>= \case
      [] -> return ()
      _ -> do
        checkAvoids
        retireStmt
        drainRemainingStmts

data MatcherResult
  = MatchNoAssertions [Pil.Stmt]
  | MatchWithAssertions [Pil.Stmt] -- need to run solver on them
  | NoMatch
  | UnboundVariableError Symbol
  deriving (Eq, Ord, Show, Hashable, Generic)

-- | Matches list of statements with pattern. Returns new list of statements
-- that may include added assertions.
matchStmts :: [StmtPattern] -> [Pil.Stmt] -> (MatcherState, MatcherResult)
matchStmts pats stmts = case runMatchStmts pats stmts of
  (ms, False) -> (ms, NoMatch)
  (ms, True) -> (ms,) $ case getStmtsWithResolvedBounds ms of
    Left (CannotFindBoundVarInState sym) -> UnboundVariableError sym
    Right (0, stmts') -> MatchNoAssertions $ stmts' <> ms ^. #remainingStmts
    Right (_, stmts') -> MatchWithAssertions $ stmts' <> ms ^. #remainingStmts

matchStmts' :: [StmtPattern] -> [Pil.Stmt] -> MatcherResult
matchStmts' pats = snd . matchStmts pats

matchPath :: [StmtPattern] -> PilPath -> (MatcherState, MatcherResult)
matchPath pat = matchStmts pat
  . resolveCalls
  . PA.aggressiveExpand
  . Path.toStmts  
