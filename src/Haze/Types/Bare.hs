{-# LANGUAGE TemplateHaskell #-}

module Haze.Types.Bare where

-- import Hinja.Core (Address, InstructionIndex)
-- import Hinja.Function (Function)
-- import Hinja.MLIL (Expression, OperationSize)
-- import qualified Hinja.MLIL as MLIL
import Haze.Prelude hiding (Ptr)
-- import Data.Set as Set

data Ptr = Ptr Expr
  deriving (Eq, Ord, Show)

newtype Variable = Variable Text
  deriving (Eq, Ord, Show, IsString)

data Expr
  = SInt Int
  | UInt Int
  | String String

  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | UAdd Expr Expr

  | Equal Expr Expr
  | Not Expr
  | NotEqual Expr Expr
  | LessThan Expr Expr
  | SLTE Expr Expr

  | ZX Expr

  | Load Expr
  | AddressOf Variable
  | VarField Variable Int
  | Var Variable

  | Call Int [Expr]
  deriving (Eq, Ord, Show)

data Stmt
  = Return Expr
  | Store Expr Expr
  | Def Variable Expr
  | Cond Expr
--  | IfThenElse Expr [Stmt] [Stmt]
  deriving (Eq, Ord, Show)

newtype TypeVar = TypeVar Text
  deriving (Eq, Ord, Show, IsString)

newtype Pointer = Pointer Expr
  deriving (Eq, Ord, Show)

data Type = TUInt
          | TInt
          | TString
          | TPtr Pointer
          | TStruct (Map Word64 Type)
          | TVar TypeVar
          | TProc [Type] Type
          deriving (Eq, Ord, Show)

data Scheme = Scheme [TypeVar] Type
type Context = Map Variable Scheme
type MemContext = Map Pointer Scheme
type Substitution = Map TypeVar Type

type TT a = State Int a


-- infer :: Expr -> Scheme


folderGetExprVars :: Expr -> FolderAction [Variable]
folderGetExprVars (AddressOf v) = StopWith [v]
folderGetExprVars (VarField v _) = StopWith [v]
folderGetExprVars (Var v) = StopWith [v]
folderGetExprVars _ = RecurWithout

folderGetStmtVars :: Stmt -> FolderAction [Variable]
folderGetStmtVars (Def v _) = RecurWith [v]
folderGetStmtVars _ = RecurWithout

foldMapStmt :: Monoid m => (Stmt -> FolderAction m) -> (Expr -> FolderAction m) -> Stmt -> m
foldMapStmt g f stmt = case g stmt of
  RecurWith x -> x <> recur
  RecurWithout -> recur
  StopWith x -> x
  StopWithout -> mempty
  where
    recur = case stmt of
      (Return a) -> foldMapExpr f a
      (Store a b) -> foldMapExpr f a <> foldMapExpr f b
      (Def _ a) -> foldMapExpr f a
      (Cond a) -> foldMapExpr f a

foldMapExpr :: Monoid m => (Expr -> FolderAction m) -> Expr -> m
foldMapExpr f expr = case f expr of
  RecurWith x -> x <> recur
  RecurWithout -> recur
  StopWith x -> x
  StopWithout -> mempty
  where
    recur = case expr of
      (Add a b) -> foldMapExpr f a <> foldMapExpr f b
      (Mul a b) -> foldMapExpr f a <> foldMapExpr f b
      (UAdd a b) -> foldMapExpr f a <> foldMapExpr f b
      (Equal a b) -> foldMapExpr f a <> foldMapExpr f b
      (NotEqual a b) -> foldMapExpr f a <> foldMapExpr f b
      (LessThan a b) -> foldMapExpr f a <> foldMapExpr f b
      (SLTE a b) -> foldMapExpr f a <> foldMapExpr f b
      (Not a) -> foldMapExpr f a
      (ZX a) -> foldMapExpr f a
      (Load a) -> foldMapExpr f a
      (Call _ xs) -> foldMap (foldMapExpr f) xs
      _ -> mempty

data Bill
  = Time Bill Int
  | BadBoy
  | Liver Text Bill Bill
  deriving (Eq, Ord, Show)

bcakes :: Bill
bcakes = Time (Liver "Jimmy" (Time BadBoy 18) (Liver "Tanks" BadBoy BadBoy)) 3

data FolderAction a = RecurWith a
                    | RecurWithout
                    | StopWith a
                    | StopWithout
                    deriving (Eq, Ord, Show)

billTimes :: Bill -> FolderAction [Int]
billTimes (Time _ n) = RecurWith [n]
billTimes _ = RecurWithout

-- billTimes' :: Bill -> [Int] -> FolderAction [Int]
-- billTimes' (Time _ n) xs = RecurWith (n:xs)
-- billTimes' _ _ = RecurWithout

-- folderBill :: (Bill -> a -> FolderAction a) -> a -> Bill -> a
-- folderBill f acc bill = case f bill acc of
--   RecurWith x -> recur x
--   RecurWithout -> recur acc
--   StopWith x -> x
--   StopWithout -> acc
--   where
--     recur acc' = case bill of
--       (Time a _) -> folderBill f acc' a
--       BadBoy -> acc'
--       Liver _ a b -> folderBill f (folderBill f acc' b) a 


foldMapBill :: Monoid m => (Bill -> FolderAction m) -> Bill -> m
foldMapBill f bill = case f bill of
  RecurWith x -> x <> recur
  RecurWithout -> recur
  StopWith x -> x
  StopWithout -> mempty
  where
    recur = case bill of
      (Time a _) -> foldMapBill f a
      BadBoy -> mempty
      Liver _ a b -> foldMapBill f a <> foldMapBill f b

prog :: [Stmt]
prog =
  [ Def "x" (SInt 35)
  , Def "y" (SInt 88)
  , Def "bb" (SInt 384238432)
  , Store (AddressOf "bb") (Add (Var "x") (Var "y"))
  , Def "z" (Mul (SInt 4) (Load (Var "bb")))
  , Return (Var "z")
  ]



prog2 :: [Stmt]
prog2 =
  [ Def "arg1#0_@_create_account.1" (UInt 555)
  , Def "arg2#0_@_create_account.1" (UInt 3000)
  , Def "arg3#0_@_main.0" (Var "arg3#0_@_main.0")
  , Def "var_14#1_@_create_account.1" (VarField "arg1#0_@_create_account.1" 0)
  , Def "var_18#1_@_create_account.1" (VarField "arg2#0_@_create_account.1" 0)
  , Def "rax#1_@_create_account.1" (Call 4294971194 [UInt 8, Var "arg2#0_@_create_account.1", Var "arg3#0_@_main.0", UInt 8])
  , Cond (NotEqual (Var "rax#1_@_create_account.1") (UInt 0))
  , Def "rax_1#2_@_create_account.1" (ZX (Var "var_14#1_@_create_account.1"))
  , Store (AddressOf "rax#1_@_create_account.1") (VarField "rax_1#2_@_create_account.1" 0)
  , Def "rax_2#3_@_create_account.1" (ZX (Var "var_18#1_@_create_account.1"))
  , Store (UAdd (AddressOf "rax#1_@_create_account.1") (UInt 4)) (VarField "rax_2#3_@_create_account.1" 0)
-- Exiting _create_account[1]...
-- Entering _create_account[2]...
  , Def "arg1#0_@_create_account.2" (UInt 666)
  , Def "arg2#0_@_create_account.2" (UInt 3000)
  , Def "rdx#1_@_main.0" (Var "rdx#1_@_main.0")
  , Def "var_14#1_@_create_account.2" (VarField "arg1#0_@_create_account.2" 0)
  , Def "var_18#1_@_create_account.2" (VarField "arg2#0_@_create_account.2" 0)
  , Def "rax#1_@_create_account.2" (Call 4294971194 [UInt 8, Var "arg2#0_@_create_account.2", Var "rdx#1_@_main.0", UInt 8])
  , Cond (NotEqual (Var "rax#1_@_create_account.2") (UInt 0))
  , Def "rax_1#2_@_create_account.2" (ZX (Var "var_14#1_@_create_account.2"))
  , Store (AddressOf "rax#1_@_create_account.2") (VarField "rax_1#2_@_create_account.2" 0)
  , Def "rax_2#3_@_create_account.2" (ZX (Var "var_18#1_@_create_account.2"))
  , Store (UAdd (Var "rax#1_@_create_account.2") (UInt 4)) (VarField "rax_2#3_@_create_account.2" 0)
-- Exiting _create_account[2]...
-- Entering _atm_withdraw[3]...
  , Def "arg2#0_@_atm_withdraw.3" (UInt 2999)
  , Def "var_1c#1_@_atm_withdraw.3" (VarField "arg2#0_@_atm_withdraw.3" 0)
  , Def "rsi#1_@_atm_withdraw.3" (ZX (Load (UAdd (Var "rax#1_@_create_account.1") (UInt 4))))
  , Cond (Not (SLTE (VarField "rsi#1_@_atm_withdraw.3" 0) (Var "var_1c#1_@_atm_withdraw.3")))
  , Def "rsi_1#2_@_atm_withdraw.3" (ZX (Var "var_1c#1_@_atm_withdraw.3"))
-- Entering _withdraw[4]...
  , Def "var_14#1_@_withdraw.4" (VarField "rsi_1#2_@_atm_withdraw.3" 0)
  , Def "rsi#1_@_withdraw.4" (ZX (Var "var_14#1_@_withdraw.4"))
  , Def "rcx#1_@_withdraw.4" (ZX (Load (UAdd (Var "rax#1_@_create_account.1") (UInt 4))))
  , Def "rcx_1#2_@_withdraw.4" (ZX (Sub (VarField "rcx#1_@_withdraw.4" 0) (VarField "rsi#1_@_withdraw.4" 0)))
  , Store (UAdd (AddressOf "rax#1_@_create_account.1") (UInt 4)) (VarField "rcx_1#2_@_withdraw.4" 0)
  , Def "rdi_1#2_@_withdraw.4" (ZX (VarField "rcx_1#2_@_withdraw.4" 0))
  , Def "rax#1_@_withdraw.4" (Call 4294970336 [Var "rdi_1#2_@_withdraw.4", UInt 0])
  , Def "var_18#1_@_withdraw.4" (VarField "rax#1_@_withdraw.4" 0)
  , Def "rax_1#2_@_withdraw.4" (ZX (Var "var_18#1_@_withdraw.4"))
  , Store (UAdd (AddressOf "rax#1_@_create_account.1") (UInt 4)) (VarField "rax_1#2_@_withdraw.4" 0)
  ]

--getVars :: 

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
