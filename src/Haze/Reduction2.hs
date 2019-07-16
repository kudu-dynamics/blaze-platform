{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Haze.Reduction2 where

import Haze.Prelude hiding ((:*:))
import Data.Functor.Foldable
import Data.Functor.Foldable.TH (makeBaseFunctor)

data Expr
  = LInt Int
  | LBool Bool
  | Var Text
  | Expr :+: Expr
  | Expr :-: Expr
  | Expr :*: Expr
  | Expr :/: Expr
  | Expr :<: Expr
  | Expr :==: Expr
  | Expr :>: Expr
  | Expr :||: Expr
  | Expr :&&: Expr
  deriving (Eq, Ord, Show)

makeBaseFunctor ''Expr

--emap :: (Expr -> Expr) -> Expr -> Expr
emap :: (Corecursive a, Recursive a) => (a -> a) -> a -> a
emap f expr = embed $ fmap (emap f) (project (f expr))

expr0 :: Expr
expr0 = (LInt 3) :+: ((Var "x") :*: (LInt 0))

expr1 :: Expr
expr1 = (LInt 3 :+: Var "x")
    :<: ((Var "y" :*: LInt 0)
         :+: (LInt 72 :+: LInt 5 :+: (Var "y" :*: LInt 1)))

simplify :: (Expr -> Expr) -> Expr -> Expr
simplify rulesf expr = if expr' == expr
  then expr
  else simplify rulesf expr'
  where
    expr' = emap rulesf expr

rules :: Expr -> Expr

rules ((LInt 0) :*: _) = LInt 0
rules (_ :*: (LInt 0)) = LInt 0

rules ((LInt 0) :+: x) = x
rules (x :+: (LInt 0)) = x

rules ((LInt 1) :*: x) = x
rules (x :*: (LInt 1)) = x

rules ((LInt a) :*: (LInt b)) = LInt $ a * b
rules ((LInt a) :+: (LInt b)) = LInt $ a + b

rules expr = expr

