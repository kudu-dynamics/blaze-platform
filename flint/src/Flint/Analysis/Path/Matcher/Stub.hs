module Flint.Analysis.Path.Matcher.Stub where

import Flint.Prelude hiding (sym)

import Flint.Analysis.Path.Matcher
import Flint.Types.Analysis.Path.Matcher.PathPrep (MkPathPrep(mkPathPrep))
import Flint.Types.Symbol (Symbol)

import Blaze.Pil.Construct (ExprConstructor(..))
import qualified Blaze.Pil.Construct as C
import qualified Blaze.Types.Pil as Pil
import qualified Blaze.Types.Cfg.Path as Path
import Blaze.Types.Cfg.Path (PilPath)
import Blaze.Types.Pil.Solver (SolverResult(Sat))

import qualified Data.HashMap.Strict as HashMap


data StubBoundExpr
  = StubBound (Symbol Pil.Expression)
  | StubBoundExpr BoundExprSize (Pil.ExprOp StubBoundExpr)
  | StubNewVar (Symbol Pil.Expression) BoundExprSize
  deriving (Eq, Ord, Show, Hashable, Generic)

instance BoundVar StubBoundExpr where
  bound = StubBound

instance ExprConstructor BoundExprSize StubBoundExpr where
  mkExpr = StubBoundExpr

newVar :: Symbol Pil.Expression -> BoundExprSize -> StubBoundExpr
newVar = StubNewVar

newtype StubMatcherState = StubMatcherState
  { nextNewVarId :: Int
  }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)

type StubMatcher expr stmt a = MatcherT expr stmt (State StubMatcherState) a

runStubMatcher_
  :: forall expr stmt a.
     ( MkPathPrep stmt [stmt]
     )
  => StubMatcher expr stmt a
  -> StubMatcherState
  -> (Maybe (a, MatcherState expr stmt), StubMatcherState)
runStubMatcher_ m s = flip runState s . observeMatcherT mctx mstate $ m
  where
    solver _ = return $ Sat HashMap.empty
    (mctx, mstate) = mkMatcherState solver $ mkPathPrep [] ([] :: [stmt])
  
resolveStubBoundExpr
  :: IsExpression expr
  => StubBoundExpr
  -> StubMatcher expr stmt expr
resolveStubBoundExpr (StubBound sym) = lookupBound sym
resolveStubBoundExpr (StubBoundExpr bsize op) = do
  mkExprWithSize <$> resolveBoundExprSize bsize <*> traverse resolveStubBoundExpr op
resolveStubBoundExpr (StubNewVar sym sz) = do
  sz' <- resolveBoundExprSize sz
  vid <- lift $ use #nextNewVarId
  let v = liftVar . C.pilVar (coerce sz') $ cs sym <> "_" <> show vid
  #boundSyms %= HashMap.insert sym v
  lift $ #nextNewVarId %= (+1)
  return v

-- | Specifies a list of stubbed statements to replace a matching statement with.
-- Typically, this will be used to replace calls.
-- Expressions bound in `stmtToStub` can be used in the stubs.
data StubSpec = StubSpec
  { stmtToStub :: Statement ExprPattern
  , removeOriginalStmt :: Bool
  , stubs :: [Pil.Statement StubBoundExpr]
  } deriving (Eq, Ord, Show, Hashable, Generic)



-- | Replaces a matching statement with stubbed statements.
-- nextVarId is passed in and returned in first arg of tuple
-- and is used to ensure NewVars are unique.
stubStmt
  :: ( HasAddress stmt
     , IsStatement expr stmt
     , IsExpression expr
     , MkPathPrep stmt [stmt]
     )
  => StubMatcherState
  -> StubSpec
  -> stmt
  -> Maybe (StubMatcherState, [stmt])
stubStmt st sspec stmt = case matcherResult of
  (Nothing, _) -> Nothing
  (Just ( stmts, _), st') -> Just (st', stmts)
  where
    matcherResult = flip runStubMatcher_ st $ do
      matchStmt (sspec ^. #stmtToStub) stmt
      resolvedStmts <- traverse (traverse resolveStubBoundExpr) $ sspec ^. #stubs
      return $ bool [stmt] [] (sspec ^. #removeOriginalStmt) <> (mkStmtLike stmt <$> resolvedStmts)

stubStmts :: [StubSpec] -> [Pil.Stmt] -> [Pil.Stmt]
stubStmts specs = go (StubMatcherState 0)
  where
    go _ [] = []
    go st (stmt:stmts) =
      case headMay $ mapMaybe (\spec -> stubStmt st spec stmt) specs of
        Nothing -> stmt : go st stmts
        Just (st', stubbedStmts) -> stubbedStmts <> go st' stmts

stubPath :: [StubSpec] -> PilPath -> PilPath
stubPath spec = Path.safeMap (stubStmts spec <$>)
