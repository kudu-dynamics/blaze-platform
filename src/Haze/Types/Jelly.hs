{-# LANGUAGE TemplateHaskell #-}

module Haze.Types.Jelly where

-- import Hinja.Core (Address, InstructionIndex)
-- import Hinja.Function (Function)
-- import Hinja.MLIL (Expression, OperationSize)
-- import qualified Hinja.MLIL as MLIL
import Haze.Prelude
-- import Data.Set as Set

data Expression = Expression
  { size :: Int
  , op :: ExprOp (Expression)
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

countSizes :: ExprOp Expression -> Int
countSizes = foldr (\ x acc -> size x + countSizes (op x) + acc) 0

