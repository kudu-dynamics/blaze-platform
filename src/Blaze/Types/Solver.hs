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
import qualified Data.SBV.Dynamic as SBV
import qualified Data.SBV.Internals as SBV
import Data.SBV.Trans ((.&&))
import Data.SBV.Trans as Exports hiding (Solver, checkSat, CheckSatResult, SMTResult)
import Data.SBV.Trans.Control as Exports hiding (Solver, checkSat, CheckSatResult, SMTResult, Sat, Unk, Unsat)
import Blaze.Types.Pil (Expression, Stmt, PilVar, TypeEnv)
import qualified Data.HashMap.Strict as HashMap
import Data.SBV.Internals (SolverContext(..))
import qualified Blaze.Types.Pil as Pil
import qualified Z3.Monad as Z3
import Z3.Monad (Z3, MonadZ3)
import GHC.Natural (Natural)

ex34 :: Z3 Z3.Result
ex34 = do
  a <- Z3.mkTrue
  b <- Z3.mkBoolVar =<< Z3.mkStringSymbol "b"
  c <- Z3.mkAnd [a, b]
  s1 <- Z3.mkString "Hello there"
  s2 <- Z3.mkSeqEmpty =<< Z3.mkStringSort
  d <- Z3.mkEq s1 s2
  Z3.solverAssertCnstr d
  Z3.solverAssertCnstr c
  (r, mm) <- Z3.solverCheckAndGetModel
  case mm of
    Nothing -> return ()
    Just m -> do
      b' <- Z3.evalBool m b
      print b'
      Z3.getString s1 >>= print
  return r

data SolverError = SymVarConversionError PilVar Pil.Type SymVarConversionError
                 | OpNotYetSupported Expression
                 | UknownExpectedType Expression
                 | UnexpectedReturnType
                 | IntegralConversionError
                 | ArgsAndRetNotTheSameType
                 | UnrecognizedTypeWidth Int
                 | ExpectedTypeWidth
                 | SignExtendResultMustBeWiderThanArgument


data SymVarConversionError = UnrecognizedWordWidth Int
                           | UnrecognizedIntWidth Int
                           | ArrayTypeNotYetSupported
                           | FieldTypeNotYetSupported
                           | EncounteredObsType
                           | FuncTypeNotYetSupported


data SymExpr = SymBool SBool
             | SymWord8 SWord8
             | SymWord16 SWord16
             | SymWord32 SWord32
             | SymWord64 SWord64
             | SymInt8 SInt8
             | SymInt16 SInt16
             | SymInt32 SInt32
             | SymInt64 SInt64
             | SymFloat SFloat
             -- SymArray (SArray)
             | SymString SString
             deriving (Eq, Show)

class SameType a b where
  sameType :: a -> b -> Bool

--- associativity
--- sameType a b && sameType b c => sameType a c

instance SameType SymExpr SymExpr where
  sameType (SymBool _) (SymBool _) = True
  sameType (SymWord8 _) (SymWord8 _) = True
  sameType (SymWord16 _) (SymWord16 _) = True
  sameType (SymWord32 _) (SymWord32 _) = True
  sameType (SymWord64 _) (SymWord64 _) = True
  sameType (SymInt8 _) (SymInt8 _) = True
  sameType (SymInt16 _) (SymInt16 _) = True
  sameType (SymInt32 _) (SymInt32 _) = True
  sameType (SymInt64 _) (SymInt64 _) = True
  sameType (SymFloat _) (SymFloat _) = True
  sameType (SymString _) (SymString _) = True
  sameType _ _ = False

instance SameType SymExpr Pil.Type where
  sameType (SymBool _) Pil.TBool = True

  sameType (SymWord8 _) (Pil.TInt x) = not (x ^. Pil.signed) && x ^. Pil.width == 8
  sameType (SymWord8 _) (Pil.TBitVec x) = x ^. Pil.width == 8
  sameType (SymWord8 _) (Pil.TPtr x) = x ^. Pil.width == 8
  sameType (SymWord16 _) (Pil.TInt x) = not (x ^. Pil.signed) && x ^. Pil.width == 16
  sameType (SymWord16 _) (Pil.TBitVec x) = x ^. Pil.width == 16
  sameType (SymWord16 _) (Pil.TPtr x) = x ^. Pil.width == 16
  sameType (SymWord32 _) (Pil.TInt x) = not (x ^. Pil.signed) && x ^. Pil.width == 32
  sameType (SymWord32 _) (Pil.TBitVec x) = x ^. Pil.width == 32
  sameType (SymWord32 _) (Pil.TPtr x) = x ^. Pil.width == 32
  sameType (SymWord64 _) (Pil.TInt x) = not (x ^. Pil.signed) && x ^. Pil.width == 64
  sameType (SymWord64 _) (Pil.TBitVec x) = x ^. Pil.width == 64
  sameType (SymWord64 _) (Pil.TPtr x) = x ^. Pil.width == 64

  sameType (SymInt8 _) (Pil.TInt x) = x ^. Pil.signed && x ^. Pil.width == 8
  sameType (SymInt16 _) (Pil.TInt x) = x ^. Pil.signed && x ^. Pil.width == 16
  sameType (SymInt32 _) (Pil.TInt x) = x ^. Pil.signed && x ^. Pil.width == 32
  sameType (SymInt64 _) (Pil.TInt x) = x ^. Pil.signed && x ^. Pil.width == 64

  -- TODO: there's only one symfloat type, and several TFloat types
  sameType (SymFloat _) (Pil.TFloat _) = True
  sameType (SymString _) Pil.TString = True
  sameType _ _ = False


instance SameType Pil.Type SymExpr where
  sameType = flip sameType

-- data SymExpr a where
--   SymBool :: SBool -> SymExpr SBool
--   SymWord8 :: SWord8 -> SymExpr SWord8
--   SymWord16 :: SWord16 -> SymExpr SWord16
--   SymWord32 :: SWord32 -> SymExpr SWord32
--   SymWord64 :: SWord64 -> SymExpr SWord64
--   SymInt8 :: SInt8 -> SymExpr SInt8
--   SymInt16 :: SInt16 -> SymExpr SInt16
--   SymInt32 :: SInt32 -> SymExpr SInt32
--   SymInt64 :: SInt64 -> SymExpr SInt64
--   SymFloat :: SFloat -> SymExpr SFloat
--   -- SymArray ::  (SArray)
--   SymString :: SString -> SymExpr SString
-- -- deriving (Eq, Show)


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
