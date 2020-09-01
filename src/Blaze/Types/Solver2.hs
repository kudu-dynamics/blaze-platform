{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Blaze.Types.Solver2
  ( module Exports
  , module Blaze.Types.Solver2
  -- , liftSolverT
  -- , emptyState
  -- , emptyCtx
  -- , checkSat
  -- , checkSatWithSolution
  ) where

import Blaze.Prelude

import Data.SBV.Internals (SBV (unSBV), SolverContext (..), modelAssocs)
import Data.SBV.Dynamic (SVal, CV)
import qualified Data.SBV.Trans as SBV
import Data.SBV.Trans as Exports (SymbolicT
                                 , MonadSymbolic
                                 , SMTConfig
                                 , SBool
                                 , runSMT
                                 , runSMTWith
                                 
                                 )
import Data.SBV.Trans.Control as Exports (ExtractIO)
import qualified Data.SBV.Trans.Control as Q
import Blaze.Types.Pil (Expression, PilVar, TypeEnv)
import qualified Data.HashMap.Strict as HashMap
import qualified Blaze.Types.Pil as Pil
import Control.Monad.Fail (MonadFail(fail))
import Blaze.Types.Pil.Checker (DeepSymType, Sym, SymInfo)
import qualified Blaze.Types.Pil.Checker as Ch

type StmtIndex = Int

-- data ExpressionError = ExpressionError
--   { _currentExprSym :: Sym
--   , _errorType :: Text
--   } deriving (Eq, Ord, Read, Show)
-- $(makeFieldsNoPrefix ''SolverError)

-- data StatementError = StatementError
--   { _currentStmtIndex :: Int
--   , _currentExprSym :: Maybe Sym
--   , _errorType :: Text
--   } deriving (Eq, Ord, Read, Show)
-- $(makeFieldsNoPrefix ''SolverError)


-- data ExpressionError = ExpressionError Sym Text
--   deriving (Eq, Ord, Read, Show, Generic)

-- data ExpressionError = ExpressionError Sym Text
--   deriving (Eq, Ord, Read, Show, Generic)

type DSTExpression = Ch.InfoExpression (SymInfo, Maybe DeepSymType)

data SolverError = DeepSymTypeConversionError { deepSymType :: DeepSymType, msg ::  Text }
                 | StmtError { stmtIndex :: Int, stmtErr :: SolverError }
                 | ExprError { exprSym :: Sym, exprErr :: SolverError}
                 | GuardError { name :: Text, kinds :: [SBV.Kind], msg :: Text }
                 | AlternativeEmpty
                 | ErrorMessage Text
  deriving (Eq, Ord, Show, Generic)

-- data SolverError = SolverError
--   { _currentStmtIndex :: Int
--   , _currentExprSym :: Maybe Sym
--   , _errorType :: Text
--   } deriving (Eq, Ord, Read, Show)
-- $(makeFieldsNoPrefix ''SolverError)

class SameType a b where
  sameType :: a -> b -> Bool

--- associativity
--- sameType a b && sameType b c => sameType a c


-- instance SameType SymExpr Pil.Type where
--   sameType (SymBool _) Pil.TBool = True

--   sameType (SymWord8 _) (Pil.TInt x) = not (x ^. Pil.signed) && x ^. Pil.width == 1
--   sameType (SymWord8 _) (Pil.TBitVec x) = x ^. Pil.width == 1
--   sameType (SymWord8 _) (Pil.TPtr x) = x ^. Pil.width == 1
  
--   sameType (SymWord16 _) (Pil.TInt x) = not (x ^. Pil.signed) && x ^. Pil.width == 2
--   sameType (SymWord16 _) (Pil.TBitVec x) = x ^. Pil.width == 2
--   sameType (SymWord16 _) (Pil.TPtr x) = x ^. Pil.width == 2
  
--   sameType (SymWord32 _) (Pil.TInt x) = not (x ^. Pil.signed) && x ^. Pil.width == 4
--   sameType (SymWord32 _) (Pil.TBitVec x) = x ^. Pil.width == 4
--   sameType (SymWord32 _) (Pil.TPtr x) = x ^. Pil.width == 4
  
--   sameType (SymWord64 _) (Pil.TInt x) = not (x ^. Pil.signed) && x ^. Pil.width == 8
--   sameType (SymWord64 _) (Pil.TBitVec x) = x ^. Pil.width == 8
--   sameType (SymWord64 _) (Pil.TPtr x) = x ^. Pil.width == 8

--   sameType (SymWord128 _) (Pil.TInt x) = not (x ^. Pil.signed) && x ^. Pil.width == 16
--   sameType (SymWord128 _) (Pil.TBitVec x) = x ^. Pil.width == 16
--   sameType (SymWord128 _) (Pil.TPtr x) = x ^. Pil.width == 16

--   sameType (SymInt8 _) (Pil.TInt x) = x ^. Pil.signed && x ^. Pil.width == 1
--   sameType (SymInt16 _) (Pil.TInt x) = x ^. Pil.signed && x ^. Pil.width == 2
--   sameType (SymInt32 _) (Pil.TInt x) = x ^. Pil.signed && x ^. Pil.width == 4
--   sameType (SymInt64 _) (Pil.TInt x) = x ^. Pil.signed && x ^. Pil.width == 8
--   sameType (SymInt128 _) (Pil.TInt x) = x ^. Pil.signed && x ^. Pil.width == 16

--   -- TODO: there's only one symfloat type, and several TFloat types
--   sameType (SymFloat _) (Pil.TFloat _) = True
--   sameType (SymString _) Pil.TString = True
--   sameType _ _ = False

-- instance SameType Pil.Type SymExpr where
--   sameType = flip sameType

               
newtype SolverCtx = SolverCtx
  { typeEnv :: HashMap PilVar DeepSymType }

emptyCtx :: SolverCtx
emptyCtx = SolverCtx mempty

data SolverState = SolverState
  { _varMap :: HashMap PilVar SVal
  , _varNames :: HashMap PilVar Text
  , _mem :: HashMap Expression SVal
  , _currentStmtIndex :: Int
  , _errors :: [SolverError]
  }
$(makeFieldsNoPrefix ''SolverState)

emptyState :: SolverState
emptyState = SolverState HashMap.empty HashMap.empty HashMap.empty 0 []

-- newtype Solver a = Solver { runSolverMonad_ ::
--                               ReaderT SolverCtx
--                                 (StateT SolverState
--                                  (ExceptT SolverError
--                                   (SymbolicT IO))) a }
--                    deriving ( Functor, Applicative, Monad
--                             , MonadError SolverError
--                             , MonadReader SolverCtx
--                             , MonadState SolverState
--                             , MonadIO
--                             , MonadSymbolic
--                             )

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

-- stmtError :: Text -> Solver a
-- stmtError msg = do
--   ix <- use currentStmtIndex
--   throwError . SolverError ix Nothing $ cs msg

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

-- -- failed attempt at runSolver with Solver monad that has `SymbolicT IO`
-- runSolver :: forall a b. (SymbolicT IO a -> IO b)
--           -> (SolverState, SolverCtx)
--           -> Solver a
--           -> IO (Either SolverError b)
-- runSolver unwrapper (st, ctx) =
--   f . runExceptT . flip evalStateT st . flip runReaderT ctx . runSolverMonad_
--   where
--     f :: SymbolicT IO (Either SolverError a) -> IO (Either SolverError b)
--     f m = undefined -- impossible?

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

-- (Right (SBV.SatResult (SBV.Satisfiable _ r))) <- runSolver_ (SBV.satWith z3) test

-- checkSatWith :: SMTConfig -> Solver () -> IO (Either SolverError SolverResult)
-- checkSatWith cfg m = do
--   r <- runSolver_ (SBV.satWith cfg) m
--   case r of
--     Left err -> return $ Left err
--     Right (SBV.SatResult sr) -> return . Right $ case sr of
--       SBV.Satisfiable _ model -> Sat . HashMap.fromList $ over _1 cs <$> modelAssocs model
--       SBV.Unsatisfiable _ _ -> Unsat
--       _ -> Unk


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
checkSatWith cfg m s = runSolverWith cfg (m >> querySolverResult) s

checkSatWith_ :: SMTConfig -> Solver () -> IO (Either SolverError SolverResult)
checkSatWith_ cfg m = fmap fst <$> checkSatWith cfg m (emptyState, emptyCtx)


stmtError :: SolverError -> Solver a
stmtError e = do
  si <- use currentStmtIndex
  throwError $ StmtError si e

liftSymbolicT :: SymbolicT (ExceptT SolverError IO) a -> Solver a
liftSymbolicT = Solver . lift . lift

-- getSolution :: MonadIO m => SymExpr -> QueryT m VarVal
-- getSolution (SymBool x) = VBool <$> getValue x
-- getSolution (SymWord8 x) = VWord8 <$> getValue x
-- getSolution (SymWord16 x) = VWord16 <$> getValue x
-- getSolution (SymWord32 x) = VWord32 <$> getValue x
-- getSolution (SymWord64 x) = VWord64 <$> getValue x
-- getSolution (SymWord128 x) = VWord128 <$> getValue x
-- getSolution (SymInt8 x) = VInt8 <$> getValue x
-- getSolution (SymInt16 x) = VInt16 <$> getValue x
-- getSolution (SymInt32 x) = VInt32 <$> getValue x
-- getSolution (SymInt64 x) = VInt64 <$> getValue x
-- getSolution (SymInt128 x) = VInt128 <$> getValue x
-- getSolution _ = return VarNotFound

-- getSolutions :: MonadIO m => HashMap PilVar SymExpr -> QueryT m (HashMap PilVar VarVal)
-- getSolutions m = do
--   xs <- mapM (\ (pv, x) -> (pv,) <$> getSolution x) $ HashMap.toList m
--   return $ HashMap.fromList xs

-- getIntegralWidth :: SymExpr -> Solver Bits
-- getIntegralWidth = \case
--   (SymWord8 _) -> return 8
--   (SymWord16 _) -> return 16
--   (SymWord32 _) -> return 32
--   (SymWord64 _) -> return 64
--   (SymWord128 _) -> return 128
--   (SymInt8 _) -> return 8
--   (SymInt16 _) -> return 16
--   (SymInt32 _) -> return 32
--   (SymInt64 _) -> return 64
--   (SymInt128 _) -> return 128
--   _ -> throwError IntegralConversionError

-- getIntegral :: forall a. (SIntegral a) => SymExpr -> Solver (SBV a)
-- getIntegral = \case
--   (SymWord8 x) -> r x
--   (SymWord16 x) -> r x
--   (SymWord32 x) -> r x
--   (SymWord64 x) -> r x
--   (SymWord128 x) -> r x
--   (SymInt8 x) -> r x
--   (SymInt16 x) -> r x
--   (SymInt32 x) -> r x
--   (SymInt64 x) -> r x
--   (SymInt128 x) -> r x
--   _ -> throwError IntegralConversionError
--   where
--     r :: forall m n. (SIntegral m, SIntegral n) => SBV m -> Solver (SBV n)
--     r = return . sFromIntegral

