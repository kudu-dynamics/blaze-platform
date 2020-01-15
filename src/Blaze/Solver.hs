module Blaze.Solver where

import Blaze.Prelude

import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil (Expression, Stmt, PilVar, TypeEnv(TypeEnv))
import qualified Data.HashMap.Strict as HashMap
import Blaze.Types.Solver
import qualified Data.SBV.Trans as SBV
import qualified Data.SBV.Trans.Control as SBV
import qualified Data.Text
import qualified Binja.Function as Func

add5 :: SWord16 -> SWord16
add5 n = n + 5

pilVarName :: PilVar -> Text
pilVarName pv = pv ^. Pil.symbol
  <> maybe "" (("@"<>) . view Func.name) (pv ^. Pil.func)
  <> maybe "" (("."<>) . show . f) (pv ^. Pil.ctxIndex)
  where
    f (Pil.CtxIndex n) = n

makeSymVar :: PilVar -> Pil.Type -> Solver SymExpr
makeSymVar pv pt = undefined

initVarMap :: Solver ()
initVarMap = do
  (TypeEnv te) <- typeEnv <$> ask
  let pvts = HashMap.toList te
  vars <- mapM (\ (pv, pt) -> (pv,) <$> makeSymVar pv pt) pvts
  return ()


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
  constrain $ z .== x `add` y
  constrain $ add5 x .== (y :: SWord16)
  query $ do
    csat <- SBV.checkSat
    case csat of
      SBV.Sat -> do
        xv <- getValue x
        io . putText $ "This is Jimmy: " <> show xv
      _ -> io $ putText "sorry Jim"
  return ()
  -- return $ add5 x .== y

bigtest2 :: Symbolic SBool
bigtest2 = do
  x <- exists "x"
  y <- exists "y"
  constrain $ add5 x .== (y :: SWord16)
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
