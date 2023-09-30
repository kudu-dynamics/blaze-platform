module Flint.Analysis.Path.Matcher
 ( module Flint.Analysis.Path.Matcher
 ) where

import Flint.Prelude hiding (Symbol, sym)

import qualified Blaze.Types.Pil as Pil
import qualified Blaze.Types.Function as BFunc
import Blaze.Types.Pil (Size)

import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
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

data ExprPattern
  = Expr (Pil.ExprOp ExprPattern)
  -- binds expr to Sym if pattern matches, or if Sym already exists.
  -- sees if equal to old sym val
  -- This allows you to nest more binds within ExprPattern
  | Bind Symbol ExprPattern 
  | Var Symbol -- matches prefix of var name, like "arg4" will match "arg4-7#1"
  | Contains ExprPattern -- matches if ExprPattern matches somewhere inside expr
  | Wild
  deriving (Eq, Ord, Show, Hashable, Generic)

-- This example shows a failed check where they check the commbuffer size (arg4)
-- but later access outside its range. Pseudo-pattern:
--
-- bind 'y' = [arg4]
-- if (bind 'y' < bind 'length')
-- call SmmIsBufferOutsideSmmValid(arg3, bind 'y')
-- [arg3 + (bind 'n')] = (bind "store_val") _ where (bind 'n' > bind 'length')

exampleCommBufferOobUsage :: [StmtPattern]
exampleCommBufferOobUsage =
  [ Stmt $ Def (Bind "y" Wild) (Expr . Pil.LOAD . Pil.LoadOp $ Var "arg4")
  , Unordered
    [ AnyOne
      [ Stmt $ BranchCond . Expr . Pil.CMP_ULT $ Pil.CmpUltOp (Bind "y" Wild) (Bind "max_length" Wild)
      , Stmt $ BranchCond . Expr . Pil.CMP_UGE $ Pil.CmpUgeOp (Bind "max_length" Wild) (Bind "y" Wild)
      ]
    , Stmt $ Call (Just Wild) (CallFunc (FuncName "SmmIsBufferOutsideSmmValid"))
      [ Var "arg3"
      , Bind "y" Wild
      ]
    ]
  , Stmt $ Store (Expr . Pil.ADD $ Pil.AddOp (Var "arg3") (Bind "n" Wild)) (Bind "stored_val" Wild)
  , Assert . BoundExpr (ConstSize 8) . Pil.CMP_UGT $ Pil.CmpUgtOp (Bound "n") (Bound "max_length")
  ]

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

runMatcher :: [Pil.Stmt] -> Matcher a -> Maybe (a, MatcherState)
runMatcher stmts action = case runMatcher_ action (mkMatcherState stmts) of
  (Left _, _) -> Nothing
  (Right x, s) -> Just (x, s)

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
    _ -> bad
  Contains xpat -> do
    backtrackOnError (matchExpr xpat expr)
      <|> asum (backtrackOnError . matchExpr xpat <$> toList (expr ^. #op))
  Wild -> return ()
  Expr xop -> matchExprOp xop $ expr ^. #op    

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
        (Nothing, Nothing) -> good
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
    Assert bexpr -> addBoundExpr bexpr
  where
    perhapsRecur = if tryNextStmtOnFailure
      then do
        retireStmt
        matchNextStmt pat
      else throwError ()

newtype ResolveBoundExprError = CannotFindBoundVarIntState Symbol
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)

resolveBoundExprSize
  :: HashMap Symbol Pil.Expression
  -> BoundExprSize
  -> Either ResolveBoundExprError (Size Pil.Expression)
resolveBoundExprSize _ (ConstSize sz) = Right sz
resolveBoundExprSize m (SizeOf sym) = case HashMap.lookup sym m of
  Nothing -> Left $ CannotFindBoundVarIntState sym
  Just expr -> Right $ expr ^. #size

resolveBoundExpr
  :: HashMap Symbol Pil.Expression
  -> BoundExpr
  -> Either ResolveBoundExprError Pil.Expression
resolveBoundExpr m (Bound sym) =
  maybe (Left $ CannotFindBoundVarIntState sym) Right $ HashMap.lookup sym m
resolveBoundExpr m (BoundExpr bsize op) =
  Pil.Expression <$> resolveBoundExprSize m bsize <*> traverse (resolveBoundExpr m) op

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
-- If successful, return the MatcherState, which should be used later
-- with the Solver to check the Asserts.
matchStmts_ :: [StmtPattern] -> [Pil.Stmt] -> Maybe MatcherState
matchStmts_ pats stmts = fmap snd . runMatcher stmts $ do
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
-- If Nothing, the patterns did not match.
-- If (Just (Left err)), a var in a bound expr was not bound properly.
-- If (Just (Right stmts)), the 
matchStmts :: [StmtPattern] -> [Pil.Stmt] -> MatcherResult
matchStmts pats stmts = case matchStmts_ pats stmts of
  Nothing -> NoMatch
  Just ms -> case getStmtsWithResolvedBounds ms of
    Left (CannotFindBoundVarIntState sym) -> UnboundVariableError sym
    Right (0, stmts') -> MatchNoAssertions $ stmts' <> ms ^. #remainingStmts
    Right (_, stmts') -> MatchWithAssertions $ stmts' <> ms ^. #remainingStmts
