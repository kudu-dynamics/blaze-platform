module Blaze.Solver.Experiment where

import Blaze.Prelude

import qualified Blaze.Types.Pil as Pil
import qualified Data.HashMap.Strict as HashMap
--import Blaze.Types.Solver hiding (VBool)
import qualified Data.SBV.Trans as SBV
import qualified Data.SBV.Trans.Control as SBV
import qualified Data.Text as Text
import qualified Binja.Function as Func

data RetType = RetInt
             | RetWord
             | RetBool
             deriving (Eq, Ord, Show)

data Expression = Expression
  { retType :: RetType
  , op :: ExprOp }
  deriving (Eq, Ord, Show)

data ExprOp = EInt Int
            | EWord Word64
            | EBool Bool
            | EAnd Expression Expression
            | EOr Expression Expression
            | ENot Expression
            | EEq Expression Expression
            | EAdd Expression Expression
            | EMul Expression Expression
          deriving (Eq, Ord, Show)

data ExprVal = VInt Int
             | VWord Word64
             | VBool Bool
             deriving (Eq, Ord, Show)


getIntegral :: Integral a => ExprVal -> Maybe a
getIntegral (VInt n) = Just . fromIntegral $ n
getIntegral (VWord n) = Just . fromIntegral $ n
getIntegral _ = Nothing

evalExpr :: Expression -> Maybe ExprVal
evalExpr (Expression r expr) = case expr of
  (EInt n) -> Just $ VInt n
  (EBool n) -> Just $ VBool n
  (EWord n) -> Just $ VWord n
  (EAdd a b) -> case (evalExpr a, evalExpr b) of
    (Just n, Just m) -> f n m
    -- -- case r of
    -- --   RetBool -> Nothing
    -- --   RetInt -> Just . VInt $ f n m
    -- --   RetWord -> Just . VWord $ f n m
    -- (Just (VWord n), Just (VWord m)) -> f n m
    -- (Just (VWord n), Just (VInt m)) -> case r of
    --   RetBool -> Nothing
    --   RetInt -> Just . VInt . fromIntegral $ fromIntegral n + fromIntegral m
    --   RetWord -> Just . VWord . fromIntegral $ fromIntegral n + fromIntegral m
    -- (Just (VInt n), Just (VWord m)) -> case r of
    --   RetBool -> Nothing
    --   RetInt -> Just . VInt . fromIntegral $ fromIntegral n + fromIntegral m
    --   RetWord -> Just . VWord . fromIntegral $ fromIntegral n + fromIntegral m
    where
      f :: ExprVal -> ExprVal -> Maybe ExprVal
      f n m = case r of
        RetBool -> Nothing
        RetInt -> fmap (VInt . fromIntegral) . (+) <$> getIntegral n <*> getIntegral m
        RetWord -> fmap (VWord . fromIntegral) . (+) <$> getIntegral n <*> getIntegral m

  _ -> Nothing


data TExpr a where
  TInt :: Int -> TExpr Int
  TWord :: Word64 -> TExpr Word64
  TBool :: Bool -> TExpr Bool
  TAnd :: TExpr Bool -> TExpr Bool -> TExpr Bool
  TEq :: Eq b => TExpr b -> TExpr b -> TExpr Bool
  TAdd :: (Integral a, Integral b, Integral c) => TExpr b -> TExpr c -> TExpr a

-- evalTExpr :: TExpr a -> a
-- evalTExpr (TInt n) = n
-- evalTExpr (TWord n) = n
-- evalTExpr (TBool b) = b
-- evalTExpr (TAnd a b) = evalTExpr a && evalTExpr b
-- evalTExpr (TEq a b) = evalTExpr a == evalTExpr b
-- evalTExpr (TAdd a b) = fromIntegral $ fromIntegral (evalTExpr a) + fromIntegral (evalTExpr b)


-- toTExpr :: Expression -> Maybe (TExpr a)
-- toTExpr (EInt r n) = Just $ TInt n
