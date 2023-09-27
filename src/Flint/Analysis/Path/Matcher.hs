module Flint.Analysis.Path.Matcher where

import Flint.Prelude hiding (Symbol)

import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil (Expression, Stmt, PilVar, Size)

import qualified Data.HashMap.Strict as HashMap

type Symbol = Text

data Func expr = FuncName Text
               | FuncAddress Address
  deriving (Eq, Ord, Show, Generic)

data CallDest expr = CallFunc (Func expr)
                   | CallIndirect expr
  deriving (Eq, Ord, Show, Generic)

data Statement expr
  = Def expr expr -- Def dst src; dst is always (Var PilVar) expr
  | Constraint expr
  | Store expr expr
  | EnterFunc (Func expr) -- need one that lets you access args of expanded call?
  | Call (Maybe expr) (CallDest expr) [expr]
  | BranchCond expr
  | Jump expr
  | Ret expr
  | NoRet
  | TailCall (CallDest expr) [expr]
  deriving (Eq, Ord, Show, Generic)

data StmtPattern
  = Stmt (Statement ExprPattern)
  -- | Eventually StmtPattern
  | Avoid StmtPattern
  -- | AvoidUntil Avoid
  | AnyOne [StmtPattern]  -- One match succeeds. [] immediately succeeeds.
  | Unordered [StmtPattern] -- matches all, but in no particular order.
  | Ordered [StmtPattern] -- matches all. Good for grouping and scoping Where bounds.
  | Assert BoundExpr -- Add a boolean expr constraint, using bound variables.
  deriving (Eq, Ord, Show, Generic)

data BoundExprSize
  = ConstSize (Size Pil.Expression)
  | SizeOf Symbol  -- looks up symbol to get size of expr
  deriving (Eq, Ord, Show, Generic)
  
data BoundExpr
  = Bound Symbol -- gets expression that has been bound with Bind
  | BoundExpr BoundExprSize (Pil.ExprOp BoundExpr)
  deriving (Eq, Ord, Show, Generic)

data ExprPattern
  = Expr (Pil.ExprOp ExprPattern)
  -- binds expr to Sym if pattern matches, or if Sym already exists, sees if equal to old sym val
  -- This allows you to nest more binds within ExprPattern
  | Bind Symbol ExprPattern 
  | Var Symbol -- matches prefix of var name, like "arg4" will match "arg4-7#1"
  | Contains ExprPattern -- matches if ExprPattern matches somewhere inside expr
  | Wild
  deriving (Eq, Ord, Show, Generic)

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
                            


-- type Expr = Expression

-- -- data PilParserError = PilParserError

-- data MatcherState input = MatcherState
--   { input :: input
--   , boundSyms :: HashMap Symbol Pil.Expression
--   , extraConstraints :: [Pil.Stmt]
--   } deriving Generic

-- newtype MatcherMonad input a = MatcherMonad {
--   _runMatcher :: ExceptT () (StateT (MatcherState input) Identity) a
--   }
--   deriving newtype
--     ( Functor
--     , Applicative
--     , Monad
--     , MonadError ()
--     , MonadState (MatcherState input)
--     , Alternative
--     )

-- getInput :: Matcher a a
-- getInput 

-- matchDef :: Stmt -> (PilVer -> Matcher ()) -> (Expr -> Matcher ()) -> Matcher ()
-- matchDef (Pil.Def (Pil.DefOp destVar srcExpr)) = 
  
-- def :: Matcher PilVar () -> Matcher Expr () -> Matcher Stmt ()
-- def a b = view #input >>= \case
--   Pil.Def (Pil.DefOp destVar srcExpr) -> 

-- load :: Matcher Expr () -> Matcher Expr ()
-- load = undefined

-- var :: Text -> Matcher Expr
-- var = undefined

-- bind :: Text -> Matcher Expr
-- bind = undefined

-- stmt :: Matcher Stmt -> Matcher [Stmt]
-- stmt = undefined

-- never :: Matcher Stmt -> Matcher [Stmt]
-- never = undefined

-- q :: [Matcher Stmt]

-- jackson = do
--   stmt $ def (bind "y") (load (var "arg4"))

-- data Avoid = Avoid
--   { avoid :: StmtPattern
--   , until :: StmtPattern
--   } deriving (Eq, Ord, Show, Generic)

data MatcherState = MatcherState
  { remainingStmts :: [Pil.Stmt]
  , boundSyms :: HashMap Symbol Pil.Expression
  , avoids :: [StmtPattern]
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
mkMatcherState stmts = MatcherState stmts HashMap.empty [] []

runMatcher :: [Stmt] -> Matcher a -> Maybe (a, MatcherState)
runMatcher stmts action = case runMatcher_ action (mkMatcherState stmts) of
  (Left _, _) -> Nothing
  (Right x, s) -> Just (x, s)

-- matchStmt :: Statement ExprPattern -> Matcher ()
-- matchStmt stmt = do
  
--   \case
--   Def a b -> 

-- matchStmtPattern :: StmtPattern -> Matcher ()
-- matchStmtPattern = \case
--   Stmt s -> matchStmt
--   _ -> undefined

-- addBoundExpr :: BoundExpr -> Matcher ()
-- addBoundExpr x = modify (\s -> s & #extraConstraints %~ (x:))

-- | Gets the next statement from remainingStatements
takeNextStmt :: Matcher (Maybe Pil.Stmt)
takeNextStmt = use #remainingStmts >>= \case
  [] -> return Nothing
  (x:xs) -> do
    #remainingStmts .= xs
    return $ Just x

-- | Restores a statement to the beginning of remainingStatements
restoreStmt :: Pil.Stmt -> Matcher ()
restoreStmt stmt = #remainingStmts %= (stmt:)

matchStmt :: Pil.Stmt -> Statement ExprPattern -> Matcher ()
matchStmt stmt sPat = case (stmt, sPat) of
  _ -> undefined

-- matchAvoid :: StmtPattern -> StmtPattern -> Matcher ()
-- matchAvoid avoid term = takeStmt >>= \case
--   Nothing -> return ()
--   Just stmt -> matchStmtPattern avoid

backtrackOnError :: Matcher a -> Matcher a
backtrackOnError action = do
  s <- get
  action <|> (put s >> throwError ())

addBoundExpr :: BoundExpr -> Matcher ()
addBoundExpr x = #parsedStmtsWithAssertions %= (Left x :)

storeAsParsed :: Pil.Stmt -> Matcher ()
storeAsParsed x = #parsedStmtsWithAssertions %= (Right x :)

-- | Matches the next statement with the next stmt pattern.
matchNextStmt :: StmtPattern -> Matcher ()
matchNextStmt pat = takeNextStmt >>= \case
  Nothing -> case pat of
    Stmt _ -> bad
    Avoid _ -> good
    AnyOne [] -> good
    AnyOne _ -> bad
    Unordered [] -> good
    Unordered _ -> bad
    Ordered [] -> good
    Ordered _ -> bad
    Assert boundExpr -> addBoundExpr boundExpr >> good
    where
      bad = throwError ()
      good = return ()
  Just stmt -> case pat of
    Stmt sPat -> tryError (matchStmt stmt sPat) >>= \case
      -- Matched Statement
      Right _ -> do
        storeAsParsed stmt
        return ()
      -- Stmt failed to match
      Left _ -> do
        checkAvoids
        storeAsParsed stmt
        matchNextStmt pat
    Avoid avoid -> do
      -- An Avoid pattern doesn't consume a statement
      restoreStmt stmt
      #avoids %= (avoid:)
      return ()
    AnyOne pats -> undefined
    where
      -- | Throws error if an avoid has been found.
      checkAvoids = do
        restoreStmt stmt
        avds <- use #avoids
        tryError (asum $ backtrackOnError . matchNextStmt <$> avds) >>= \case
          -- Found an avoid
          Right _ -> throwError ()
          -- No avoid found. Remove restored stmt
          Left _ -> takeNextStmt >> return ()
  
