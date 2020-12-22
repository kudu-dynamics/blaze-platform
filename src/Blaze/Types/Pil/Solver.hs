{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Blaze.Types.Pil.Solver
  ( module Exports
  , module Blaze.Types.Pil.Solver
  ) where

import Blaze.Prelude

import Data.SBV.Internals (SolverContext (..), modelAssocs)
import Data.SBV.Dynamic (SVal, CV)
import qualified Data.SBV.Trans as SBV
import Data.SBV.Trans as Exports ( SymbolicT
                                 , MonadSymbolic
                                 , SMTConfig
                                 , SBool
                                 , runSMT
                                 , runSMTWith
                                 )
import Data.SBV.Trans.Control as Exports (ExtractIO)
import qualified Data.SBV.Trans.Control as Q
import Blaze.Types.Pil (Expression, PilVar)
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.Fail (MonadFail(fail))
import Blaze.Types.Pil.Checker (DeepSymType, Sym, SymInfo)
import qualified Blaze.Types.Pil.Checker as Ch

type StmtIndex = Int

type DSTExpression = Ch.InfoExpression (SymInfo, Maybe DeepSymType)

data SolverError = DeepSymTypeConversionError { deepSymType :: DeepSymType, msg ::  Text }
                 | StmtError { stmtIndex :: Int, stmtErr :: SolverError }
                 | ExprError { exprSym :: Sym, exprErr :: SolverError}
                 | GuardError { name :: Text, kinds :: [SBV.Kind], msg :: Text }
                 | AlternativeEmpty
                 | ErrorMessage Text
  deriving (Eq, Ord, Show, Generic)

              
newtype SolverCtx = SolverCtx
  { typeEnv :: HashMap PilVar DeepSymType }

emptyCtx :: SolverCtx
emptyCtx = SolverCtx mempty

data SolverState = SolverState
  { varMap :: HashMap PilVar SVal
  , varNames :: HashMap PilVar Text
  , mem :: HashMap Expression SVal
  , currentStmtIndex :: Int
  , errors :: [SolverError]
  } deriving (Generic)

emptyState :: SolverState
emptyState = SolverState HashMap.empty HashMap.empty HashMap.empty 0 []

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
  . runSolverMonad_ $ m

runSolverWith_ :: SMTConfig
               -> Solver a
               -> IO (Either SolverError (a, SolverState))
runSolverWith_ solverCfg = flip (runSolverWith solverCfg) (emptyState, emptyCtx)


data SolverResult = Unsat
                  | Unk
                  | Sat (HashMap Text CV)
                  deriving (Eq, Ord, Show)


querySolverResult :: Solver SolverResult
querySolverResult = do
  liftSymbolicT . Q.query $ do
    csat <- Q.checkSat
    case csat of
      Q.Unsat -> return Unsat
      Q.Unk -> return Unk
      Q.Sat -> do
        model <- Q.getModel
        return $ Sat . HashMap.fromList $ over _1 cs <$> modelAssocs model

checkSatWith :: SMTConfig -> Solver () -> (SolverState, SolverCtx) -> IO (Either SolverError (SolverResult, SolverState))
checkSatWith cfg m = runSolverWith cfg (m >> querySolverResult)

checkSatWith_ :: SMTConfig -> Solver () -> IO (Either SolverError SolverResult)
checkSatWith_ cfg m = fmap fst <$> checkSatWith cfg m (emptyState, emptyCtx)

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
  
$(makeFieldsNoPrefix ''SolverReport)

