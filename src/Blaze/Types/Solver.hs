module Blaze.Types.Solver
  ( module Exports
  , Solver
  , liftSolverT
  , constrain
  , emptyState
  , emptyCtx
  , checkSat
  , checkSatWithSolution
  ) where

import Blaze.Prelude

import qualified Data.SBV.Trans as SBV
import qualified Data.SBV.Trans.Control as SBV
import Data.SBV.Trans as Exports hiding (Solver, constrain, checkSat)
import Data.SBV.Trans.Control as Exports hiding (Solver, constrain, checkSat)
import Blaze.Types.Pil (Expression, Stmt, PilVar, TypeEnv)
import qualified Data.HashMap.Strict as HashMap

type SolverError = Text

data SymExpr = SymBool SBool
             | SymWord8 SWord8
             | SymWord16 SWord16
             | SymWord32 SWord32

data SolverCtx = SolverCtx
  { typeEnv :: TypeEnv }

emptyCtx :: SolverCtx
emptyCtx = SolverCtx mempty

data SolverState = SolverState
  { varMap :: HashMap PilVar SymExpr
  , mem :: HashMap Expression SymExpr
  }

emptyState :: SolverState
emptyState = SolverState HashMap.empty HashMap.empty

newtype Solver a = Solver { runSolver_ ::
                              (ReaderT SolverCtx
                               (StateT SolverState
                                (SymbolicT (ExceptT SolverError IO))) a) }
                   deriving ( Functor, Applicative, Monad
                            , MonadError SolverError
                            , MonadReader SolverCtx
                            , MonadState SolverState
                            , MonadIO
                            , MonadSymbolic
                            )

runSolver :: (SolverState, SolverCtx) -> Solver a -> IO (Either SolverError a)
runSolver (st, ctx) = runExceptT . runSMT . flip evalStateT st . flip runReaderT ctx . runSolver_

checkSat :: (SolverState, SolverCtx) -> Solver () -> IO (Either SolverError SatResult)
checkSat s m = runSolver s $ m >>= liftSolverT . lift . sat . const sTrue

checkSatWithSolution :: (SolverState, SolverCtx) -> Solver () -> IO (Either SolverError ())
checkSatWithSolution s m = runSolver s $ do
  _ <- m
  liftSolverT . query $ do
    cs <- SBV.checkSat
    case cs of
      Sat -> do
        io . putText $ "You are SAT"
      _ -> io $ putText "sorry Jim"

    return ()

liftSolverT :: SymbolicT (ExceptT SolverError IO) a -> Solver a
liftSolverT = Solver . lift . lift

constrain :: SBool -> Solver ()
constrain = liftSolverT . SBV.constrain
