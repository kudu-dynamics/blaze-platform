module Reduction where

import Haze.Prelude hiding ((:*:))
import Data.Functor.Foldable (Fix(..), Base)

-- data Lit = LInt Int
--          | LBool Bool
--          deriving (Eq, Ord, Show)

data ExprF a
  = LInt Int
  | LBool Bool
  | Var Text
  | a :+: a
  | a :-: a
  | a :*: a
  | a :/: a
  | a :<: a
  | a :==: a
  | a :>: a
  | a :||: a
  | a :&&: a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type Expr = Fix ExprF

--newtype Term f = In { out :: f (Term f) }

-- fixAll :: f (Term f) -> Term

master :: Int -> Text
master 0 = "Good"
master _ = "bad"

expr0 :: Expr
expr0 = Fix $ (Fix $ LInt 3) :+: (Fix $ Var "x")

-- expr00 :: Expr
expr00 = (LInt 3) :+: ((Var "x") :*: (LInt 0))

expr1 = (LInt 3 :+: Var "x")
   :<: ((Var "y" :*: LInt 0) :+: (LInt 72 :+: LInt 5 :+: (Var "y" :*: LInt 1)))


