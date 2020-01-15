{-# LANGUAGE TemplateHaskell #-}
module Blaze.Types.Solver
  ( module Exports
  , module Blaze.Types.Solver
  -- , liftSolverT
  -- , emptyState
  -- , emptyCtx
  -- , checkSat
  -- , checkSatWithSolution
  ) where

import Blaze.Prelude

import qualified Data.SBV.Trans as SBV
import qualified Data.SBV.Trans.Control as SBV
import Data.SBV.Trans as Exports hiding (Solver, checkSat, CheckSatResult, SMTResult)
import Data.SBV.Trans.Control as Exports hiding (Solver, checkSat, CheckSatResult, SMTResult, Sat, Unk, Unsat)
import Blaze.Types.Pil (Expression, Stmt, PilVar, TypeEnv)
import qualified Data.HashMap.Strict as HashMap
import Data.SBV.Internals (SolverContext(..))

type SolverError = Text

data SymExpr = SymBool SBool
             | SymWord8 SWord8
             | SymWord16 SWord16
             | SymWord32 SWord32
             deriving (Eq, Show)

data VarVal = VBool Bool
            | VWord8 Word8
            | VWord16 Word16
            | VWord32 Word32
            | VarNotFound
            deriving (Eq, Ord, Show)

               
data SolverCtx = SolverCtx
  { typeEnv :: TypeEnv }

emptyCtx :: SolverCtx
emptyCtx = SolverCtx mempty

data SolverState = SolverState
  { _varMap :: HashMap PilVar SymExpr
  , _mem :: HashMap Expression SymExpr
  }
$(makeFieldsNoPrefix ''SolverState)

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

instance SolverContext Solver where
  constrain = liftSolverT . constrain
  softConstrain = liftSolverT . softConstrain
  namedConstraint s = liftSolverT . namedConstraint s
  constrainWithAttribute xs = liftSolverT . constrainWithAttribute xs
  setInfo s = liftSolverT . setInfo s
  setOption = liftSolverT . setOption
  setLogic = liftSolverT . setLogic
  setTimeOut = liftSolverT . setTimeOut
  contextState = liftSolverT contextState

runSolver :: (SolverState, SolverCtx) -> Solver a -> IO (Either SolverError a)
runSolver (st, ctx) = runExceptT . runSMT . flip evalStateT st . flip runReaderT ctx . runSolver_

checkSat :: (SolverState, SolverCtx) -> Solver () -> IO (Either SolverError SatResult)
checkSat s m = runSolver s $ m >>= liftSolverT . lift . sat . const sTrue

data SolutionResult = Unsat
                    | Unk
                    | Sat (HashMap PilVar VarVal)
                    deriving (Eq, Ord, Show)

checkSatWithSolution :: (SolverState, SolverCtx) -> Solver () -> IO (Either SolverError SolutionResult)
checkSatWithSolution s m = runSolver s $ do
  _ <- m
  vm <- use varMap
  liftSolverT . query $ do
    csat <- SBV.checkSat
    case csat of
      SBV.Unsat -> return Unsat
      SBV.Unk -> return Unk
      SBV.Sat -> do
        Sat <$> getSolutions vm

liftSolverT :: SymbolicT (ExceptT SolverError IO) a -> Solver a
liftSolverT = Solver . lift . lift

getSolution :: MonadIO m => SymExpr -> QueryT m VarVal
getSolution (SymBool x) = VBool <$> getValue x
getSolution (SymWord8 x) = VWord8 <$> getValue x
getSolution (SymWord16 x) = VWord16 <$> getValue x
getSolution (SymWord32 x) = VWord32 <$> getValue x

getSolutions :: MonadIO m => HashMap PilVar SymExpr -> QueryT m (HashMap PilVar VarVal)
getSolutions m = do
  xs <- mapM (\ (pv, x) -> (pv,) <$> getSolution x) $ HashMap.toList m
  return $ HashMap.fromList xs

-- constrain :: SBool -> Solver ()
-- constrain = liftSolverT . SBV.constrain
