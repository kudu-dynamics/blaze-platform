{-# LANGUAGE TemplateHaskell #-}

module Blaze.Types.Spice where

-- import Binja.Core (Address, InstructionIndex)
-- import Binja.Function (Function)
-- import Binja.MLIL (Expression, OperationSize)
-- import qualified Binja.MLIL as MLIL
import Blaze.Prelude

data Expr a where
  Int :: Int -> Expr Int
--  Bool :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Sub :: Expr Int -> Expr Int -> Expr Int
  Mul :: Expr Int -> Expr Int -> Expr Int
  Equals :: (Eq a) => Expr a -> Expr a -> Expr Bool
  IfThenElse :: Expr Bool -> Expr a -> Expr a -> Expr a
  BindVar :: Expr a -> (Expr a -> Expr b) -> Expr b


eval :: Expr a -> a
eval (Int n) = n
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mul a b) = eval a * eval b
eval (Equals a b) = eval a == eval b
eval (IfThenElse ch a b) = if eval ch then eval a else eval b
eval (BindVar a f) = eval $ f a


example :: Int -> Expr Int
example input = BindVar (Int input) $ \n ->
  IfThenElse (Equals n (Int 10)) (Int 0) (Add n (Int 100))
