{-# LANGUAGE TemplateHaskell #-}

module Blaze.Types.Jelly where

-- import Binja.Core (Address, InstructionIndex)
-- import Binja.Function (Function)
-- import Binja.MLIL (Expression, OperationSize)
-- import qualified Binja.MLIL as MLIL
import Blaze.Prelude
-- import Data.Set as Set

data Expression = Expression
  { size :: Int
  , op :: ExprOp (Expression)
  } deriving (Eq, Ord, Show)

data SExpression expr = SExpression
  { lozo :: Int
  , pop :: ExprOp expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)


data BiffExpression = BiffExpression
  { biffgeist :: Text
  , bop :: ExprOp BiffExpression
  } deriving (Eq, Ord, Show)

data Op expr = Op
  { masterOfPain :: expr
  , hisName :: Text }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data PhilOp expr = PhilOp
  { biff :: expr
  , hisAge :: Int }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data ExprOp expr
  =  (Op expr)
  | PHIL (PhilOp expr)
  | BADZONE
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

ex1 :: ExprOp Expression
ex1 = PHIL (PhilOp (Expression 4 jack) 84)
  where
    jack =  (Op (Expression 5 BADZONE) "Billy")

countSizes :: ExprOp Expression -> Int
countSizes = foldr (\ x acc -> size x + countSizes (op x) + acc) 0

changeToBiff :: Expression -> BiffExpression
changeToBiff e = BiffExpression (show $ size e) (changeToBiff <$> op e)
