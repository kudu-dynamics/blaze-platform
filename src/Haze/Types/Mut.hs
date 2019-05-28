{-# LANGUAGE TemplateHaskell #-}

module Haze.Types.Mut where

-- import Hinja.Core (Address, InstructionIndex)
-- import Hinja.Function (Function)
-- import Hinja.MLIL (Expression, OperationSize)
-- import qualified Hinja.MLIL as MLIL
import Haze.Prelude hiding (Ptr)

data Ptr a = Ptr (Expr Int)
  deriving (Eq, Ord, Show)

data Var a = Var Text
  deriving (Eq, Ord, Show)

data Expr a where
  Int :: Int -> Expr Int
  String :: String -> Expr String

  Add :: Expr Int -> Expr Int -> Expr Int
  Sub :: Expr Int -> Expr Int -> Expr Int
  Mul :: Expr Int -> Expr Int -> Expr Int

  Equals :: (Eq a) => Expr a -> Expr a -> Expr Bool
  LessThan :: (Eq a, Ord a) => Expr a -> Expr a -> Expr Bool

  Load :: Ptr a -> Expr a
  VarField :: Var a -> Int -> Expr b

deriving instance Eq a => Eq (Expr a)
deriving instance (Eq a, Ord a) => Ord (Expr a)
deriving instance (Show a) => Show (Expr a)

data Stmt where
  Return :: Expr a -> Stmt
  Store :: Ptr a -> Expr a -> Stmt
  Def :: Var a -> Expr a -> Stmt
--  IfThenElse :: Expr Bool -> [Stmt] -> [Stmt] -> Stmt

-- progr :: [Stmt]
-- progr =
--   [ Def "arg1#0_@_create_account.1" (Int 555)
--   , Def "arg2#0_@_create_account.1" (Int 3000)
--   , Def "var_14#1_@_create_account.1" 


-- data Stmt a where
--   Return :: Expr a -> Stmt a
--   Store :: Ptr a -> Expr a -> (Ptr a -> Stmt b) -> Stmt b
--   Def :: Var a -> Expr a -> (Var a -> Stmt b) -> Stmt b
--   IfThenElse :: Expr Bool -> Stmt a -> Stmt a -> Stmt a

-- def x = 32
-- def y = 50 + x
-- def s = "Hello"
-- call f (&s, x) -> Int
--   if (x < 50) then
--     def call overwrite &s


-- data ControlFlow a where
--   Block :: Stmt a -> ControlFlow
--   IfThenElse :: Expr Bool -> ControlFlow -> ControlFlow -> ControlFlow
  

-- evalExpr :: Expr a -> a
-- evalExpr (Int n) = n
-- evalExpr (Add a b) = evalExpr a + evalExpr b
-- evalExpr (Sub a b) = evalExpr a - evalExpr b
-- evalExpr (Mul a b) = evalExpr a * evalExpr b
-- evalExpr (Equals a b) = evalExpr a == evalExpr b
-- evalExpr (LessThan a b) = evalExpr a < evalExpr b

-- data ProgramState = 

-- newtype Program = Program { runProgram :: StateT 

-- evalStmt :: Stmt a -> a
-- evalSt

-- example :: Int -> Expr Int
-- example input = BindVar (Int input) $ \n ->
--   IfThenElse (Equals n (Int 10)) (Int 0) (Add n (Int 100))
