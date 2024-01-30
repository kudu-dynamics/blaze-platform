{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

module Blaze.Types.Pil.Solver
  ( module Exports
  , module Blaze.Types.Pil.Solver
  ) where

import Blaze.Prelude hiding (Symbol)

import Data.SBV.Internals (SolverContext (..), modelAssocs)
import Data.SBV.Dynamic (SVal)
import Data.SBV.Dynamic as Exports (CV)
import qualified Data.SBV.Trans as SBV
import Data.SBV.Trans as Exports ( SymbolicT
                                 , MonadSymbolic
                                 , SMTConfig
                                 , SBool
                                 , runSMT
                                 , runSMTWith
                                 )
import Data.SBV.Trans.Control as Exports (ExtractIO)
import Data.SBV.Trans.Control (QueryT)
import qualified Data.SBV.Trans.Control as Q
import Blaze.Types.Pil (Expression, PilVar, Symbol)
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.Fail (MonadFail(fail))
import Blaze.Types.Pil.Checker (DeepSymType, Sym, SymInfo)
import qualified Blaze.Types.Pil.Checker as Ch


type StmtIndex = Int

type DSTExpression = Ch.InfoExpression (SymInfo, Maybe DeepSymType)

data SolverError = DeepSymTypeConversionError { deepSymType :: DeepSymType, msg ::  Text }
                 | PilVarConversionError { pilVar :: Symbol, conversionError :: SolverError }
                 | StmtError { stmtIndex :: Int, stmtErr :: SolverError }
                 | ExprError { exprSym :: Sym, exprErr :: SolverError }
                 | GuardError { name :: Text, kinds :: [SBV.Kind], msg :: Text }
                 | StubbedFunctionArgError { funcName :: Text, expected :: Int, got :: Int }
                 | ConversionError { msg :: Text }
                 | AlternativeEmpty
                 | SizeOfError { kind :: SBV.Kind }
                 | ExtractError { endIndex :: Bits
                                , startIndex :: Bits
                                , kind :: SBV.Kind
                                , msg :: Text
                                }
                 | ErrorMessage Text
  deriving (Eq, Ord, Show, Generic)

type StubConstraintGen = SVal -> [SVal] -> Solver ()

-- | Specifies how lenient the solver is to errors.
-- AbortOnError means the solver will halt when a single error is encountered.
-- IgnoreErrors means the solver will try to solve as much as it can around the error,
-- which typically means ignoring a whole statement if any inner expression causes an error.
data SolverLeniency = AbortOnError
                    | IgnoreErrors
  deriving (Eq, Ord, Show, Generic)

data SolverCtx = SolverCtx
  { typeEnv :: HashMap PilVar DeepSymType
  , funcConstraintGen :: HashMap Text StubConstraintGen
  , useUnsatCore :: Bool
  , leniency :: SolverLeniency
  } deriving stock (Generic)

emptyCtx :: Bool -> SolverLeniency -> SolverCtx
emptyCtx = SolverCtx mempty mempty

data SolverState = SolverState
  { varMap :: HashMap PilVar SVal
  , varNames :: HashMap PilVar Text
  , currentStmtIndex :: Int
  , errors :: [SolverError]
  , stores :: HashMap Expression [SVal]
  } deriving (Generic)

emptyState :: SolverState
emptyState = SolverState mempty mempty 0 [] mempty

-- TODO: it would be nice if the ExceptT wrapped `SymbolicT IO`
-- so that we could use all the stuff requiring Provable instance
-- (which is only `SymbolicT IO`)
newtype Solver a = Solver { runSolverMonad_ ::
                              ReaderT SolverCtx
                                (StateT SolverState
                                 (SymbolicT (ExceptT SolverError IO))) a }
                   deriving newtype ( Functor, Applicative, Monad
                                    , MonadError SolverError
                                    , MonadReader SolverCtx
                                    , MonadState SolverState
                                    , MonadIO
                                    , MonadSymbolic
                                    )

instance Alternative Solver where
  empty = throwError AlternativeEmpty
  (<|>) a b = catchError a $ const b

instance MonadFail Solver where
  fail = throwError . ErrorMessage . cs

instance SolverContext Solver where
  addAxiom s = liftSymbolicT . addAxiom s
  addSMTDefinition s = liftSymbolicT . addSMTDefinition s
  constrain = liftSymbolicT . constrain
  softConstrain = liftSymbolicT . softConstrain
  namedConstraint s = liftSymbolicT . namedConstraint s
  constrainWithAttribute xs = liftSymbolicT . constrainWithAttribute xs
  setInfo s = liftSymbolicT . setInfo s
  setOption = liftSymbolicT . setOption
  setLogic = liftSymbolicT . setLogic
  setTimeOut = liftSymbolicT . setTimeOut
  contextState = liftSymbolicT contextState

runSolverWith :: SMTConfig
              -> Solver a
              -> (SolverState, SolverCtx)
              -> IO (Either SolverError (a, SolverState))
runSolverWith solverCfg m (st, ctx) = runExceptT
  . runSMTWith solverCfg
  . flip runStateT st
  . flip runReaderT ctx
  . runSolverMonad_
  $ do when (ctx ^. #useUnsatCore) $ do
         setOption $ Q.ProduceUnsatCores True
         when (isZ3 . SBV.name . SBV.solver $ solverCfg)
           . setOption $ Q.OptionKeyword ":smt.core.minimize" ["true"]
       m
  where
    -- No Eq instance for SBV.Solver
    isZ3 :: SBV.Solver -> Bool
    isZ3 SBV.Z3 = True
    isZ3 _ = False

runSolverWith_ :: SMTConfig
               -> Bool
               -> SolverLeniency
               -> Solver a
               -> IO (Either SolverError (a, SolverState))
runSolverWith_ solverCfg useUnsatCore' solverLeniency =
  flip (runSolverWith solverCfg) (emptyState, emptyCtx useUnsatCore' solverLeniency)



data SolverResult = Unsat (Maybe [String])
                  | Sat (HashMap Text CV)
                  | Unk
                  | Err (Either Ch.ConstraintGenError (SolverError, Ch.TypeReport))
                  deriving (Eq, Ord, Show, Generic)

type Query a = QueryT (ExceptT SolverError IO) a

toSolverResult :: Q.CheckSatResult -> Query SolverResult
toSolverResult = \case
  Q.Unsat -> fmap Unsat $ Q.getOption Q.ProduceUnsatCores >>= \case
    (Just (Q.ProduceUnsatCores True)) -> Just <$> Q.getUnsatCore
    _ -> return Nothing
  Q.Unk -> return Unk
  Q.DSat _precision -> do
    model <- Q.getModel
    return $ Sat . HashMap.fromList $ over _1 cs <$> modelAssocs model
  Q.Sat -> do
    model <- Q.getModel
    return $ Sat . HashMap.fromList $ over _1 cs <$> modelAssocs model

querySolverResult_ :: Query SolverResult
querySolverResult_ = Q.checkSat >>= toSolverResult

querySolverResult :: Solver SolverResult
querySolverResult = liftSymbolicT . Q.query $ querySolverResult_

checkSatWith :: SMTConfig -> Solver () -> (SolverState, SolverCtx) -> IO (Either SolverError (SolverResult, SolverState))
checkSatWith cfg m = runSolverWith cfg (m >> querySolverResult)

checkSatWith_ :: SMTConfig -> Bool -> SolverLeniency -> Solver () -> IO (Either SolverError SolverResult)
checkSatWith_ cfg useUnsatCore' solverLeniency m = fmap fst <$> checkSatWith cfg m (emptyState, emptyCtx useUnsatCore' solverLeniency)

stmtError :: SolverError -> Solver a
stmtError e = do
  si <- use #currentStmtIndex
  throwError $ StmtError si e

liftSymbolicT :: SymbolicT (ExceptT SolverError IO) a -> Solver a
liftSymbolicT = Solver . lift . lift

data SolverReport = SolverReport
  { result :: SolverResult
  , warnings :: [SolverError]
  } deriving (Eq, Ord, Show, Generic)
