module Blaze.Solver where

import Blaze.Prelude

import Blaze.Types.Pil (Expression, Stmt, PilVar, TypeEnv)
import qualified Data.HashMap.Strict as HashMap
import Blaze.Types.Solver
import qualified Data.SBV.Trans as SBV
import qualified Data.SBV.Trans.Control as SBV


add5 :: SWord16 -> SWord16
add5 n = n + 5


-- runSolver :: (SolverState, SolverCtx) -> Solver a -> IO (Either SolverError a)
-- runSolver (st, ctx) = runExceptT . flip evalStateT st . flip runReaderT ctx . runSMT




add :: (SIntegral a, SIntegral b, SIntegral c) => SBV a -> SBV b -> SBV c
add a b = sFromIntegral a + sFromIntegral b

uadd32 :: (SIntegral a, SIntegral b) => SBV a -> SBV b -> SBV Word32
uadd32 = add

smalltest :: SWord8 -> Symbolic SBool
smalltest x = do
  return $ x `shiftL` 3 .== 4 * (x :: SWord8)

smalltest2 :: Symbolic SBool
smalltest2 = do
  x <- exists "x"
  return $ x `shiftL` 3 .== 4 * (x :: SWord8)


bigtest :: Symbolic ()
bigtest = do
  x <- exists "x"
  y <- exists "y"
  z <- exists "z" :: Symbolic (SBV Word32)
  SBV.constrain $ z .== x `add` y
  SBV.constrain $ add5 x .== (y :: SWord16)
  query $ do
    cs <- SBV.checkSat
    case cs of
      Sat -> do
        xv <- getValue x
        io . putText $ "This is Jimmy: " <> show xv
      _ -> io $ putText "sorry Jim"
  return ()
  -- return $ add5 x .== y

bigtest2 :: Symbolic SBool
bigtest2 = do
  x <- exists "x"
  y <- exists "y"
  SBV.constrain $ add5 x .== (y :: SWord16)
  -- SBV.constrain $ add5 x .== x
  return sFalse

opy :: SWord16 -> SWord16 -> SWord32 -> Solver SBool
opy x y z = return $ z .== x `add` y

bigtest8 :: Solver SBool
bigtest8 = return sFalse

bigtest' :: Solver ()
bigtest' = do
  x <- exists "x"
  y <- exists "y"
  z <- exists "z" :: Solver (SBV Word32)
  constrain =<< opy x y z
  constrain $ add5 x .== (y :: SWord16)
--   -- query $ do
--   --   cs <- checkSat
--   --   case cs of
--   --     Sat -> do
--   --       xv <- getValue x
--   --       io . putText $ "This is Jimmy: " <> show xv
--   --     _ -> io $ putText "sorry Jim"
  return ()
  -- return $ add5 x .== y
