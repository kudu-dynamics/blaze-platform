{-# LANGUAGE TemplateHaskell #-}

module Blaze.Checker where

import qualified Prelude as P
import Blaze.Prelude hiding (Constraint, sym)
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import qualified Data.Map as Map

newtype Sym = Sym Text
  deriving (Eq, Ord, Show, IsString)

data T = TInt
       | TBool
       -- | SInt16
       -- | SInt32
       -- | SInt64
       -- | UInt16
       -- | UInt32
       -- | UInt64
--       | TVar Text
--       | TPtr PtrLib
       | TVar TSym
       | TFunc [T] T  -- name [args] ret
       deriving (Eq, Ord, Show)

data VarT t = VarT Text
  deriving (Eq, Ord, Show)

data PtrIndex = PtrIndexConst Integer
              | PtrIndexVar (VarT Integer)
              deriving (Eq, Ord, Show)

data PtrLib = PtrLib
  { ptrMap :: Map PtrIndex T
  , baseIndex :: Maybe PtrIndex
  } deriving (Eq, Ord, Show)

data LStmt = LDef Sym LExpr
           | LAnn Sym T
           | LReturn LExpr
           deriving (Eq, Ord, Show)

data LExpr = LInt Int
           | LBool Bool
           | LVar Sym
           | LFunc [Sym] LExpr  -- [args] body
           | LProc [Sym] [LStmt]
           | LApply Sym [LExpr]
           deriving (Eq, Ord, Show)

demo :: [LStmt]
demo = [ LAnn "+" $ TFunc [TInt, TInt] TInt
--       , LAnn "==" $ TFunc [TInt, TInt] TBool
--       , LDef "x" $ LInt 35
--       , LDef "x" $ LBool True
       , LDef "y" $ (LApply "+" [LVar "x", LInt 4])
       , LReturn $ LApply "==" [LVar "x", LVar "y"]
       ]

-- data Constraint = Equals C C
--   deriving (Eq, Ord, Show)

type Constraint = (T, T)

defaultLabel :: Text
defaultLabel = "var"

type Label = Text

type FuncLabel = Sym

data Arg = InputArg Int
         | RetArg
         deriving (Eq, Ord, Show)

data TSym = TSym Label Int
          | TSymFunc FuncLabel Arg
  deriving (Eq, Ord, Show)

data C = CVar TSym
       | CType T
       deriving (Eq, Ord, Show)

data CheckerError = FuncNotYetHandledError
                  | ProcNotYetHandledError
  deriving (Eq, Ord, Show)

data CheckerState = CheckerState
  { _exprMap :: Map LExpr TSym
  , _tSymCounter :: Int
  } deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''CheckerState)

newtype Checker a = Checker { runChecker_ :: ExceptT CheckerError (State CheckerState) a }
  deriving (Functor, Applicative, Monad, MonadState CheckerState, MonadError CheckerError)

runChecker :: Checker a -> (Either CheckerError a, CheckerState)
runChecker = flip runState s . runExceptT . runChecker_
  where
    s = CheckerState Map.empty 0

newTSym :: Maybe Label -> Checker TSym
newTSym mlbl = do
  n <- use tSymCounter
  tSymCounter %= (+1)
  return $ TSym lbl n
  where
    lbl = maybe defaultLabel identity mlbl

getTSym :: LExpr -> Checker TSym
getTSym expr = do
  m <- use exprMap
  case Map.lookup expr m of
    Just v -> return v
    Nothing -> do
      v <- newTSym (lbl expr)
      exprMap %= Map.insert expr v
      return v
  where
    lbl (LVar (Sym t)) = Just t
    lbl _ = Nothing

-- careful, infinite...
genFuncArgTSyms :: Sym -> [TSym]
genFuncArgTSyms funcName = TSymFunc funcName . InputArg <$> [0..]

getFuncArgConstraints :: Sym -> [T] -> [Constraint]
getFuncArgConstraints funcName args = zip (TVar <$> genFuncArgTSyms funcName) args

--- Assumptions
-- no var appears twice in dest of Def
getStmtConstraints :: LStmt -> Checker [Constraint]
getStmtConstraints (LDef sym expr) = do
  xvar <- getTSym expr
  defvar <- getTSym (LVar sym)
  cs <- getExprConstraints expr
  return $ (TVar defvar, TVar xvar) : cs
getStmtConstraints (LAnn sym t@(TFunc args ret)) = do
  defvar <- getTSym (LVar sym)
  return $ [ (TVar defvar, t)
           , (TVar $ TSymFunc sym RetArg, ret)
           ]
    <> getFuncArgConstraints sym args
    
getStmtConstraints (LAnn sym t) = do
  defvar <- getTSym (LVar sym)
  return [(TVar defvar, t)]
getStmtConstraints (LReturn expr) = getExprConstraints expr

getExprConstraints :: LExpr -> Checker [Constraint]
getExprConstraints expr = getTSym expr >>= \xvar -> case expr of
  LInt _ -> return [(TVar xvar, TInt)]
  LBool _ -> return [(TVar xvar, TBool)]
  LVar _ -> return []
  LFunc _ _ -> throwError FuncNotYetHandledError
  LProc _ _  -> throwError ProcNotYetHandledError
  LApply fname args -> do
    fsym <- getTSym (LVar fname)
    argtsyms <- mapM getTSym args
    argConstraints <- concat <$> mapM getExprConstraints args   
    return . concat $ [ [(TVar fsym, TFunc (TVar <$> argtsyms) (TVar xvar))]
                      , argConstraints
                      , zip (TVar <$> genFuncArgTSyms fname) (TVar <$> argtsyms)
                      , [(TVar $ TSymFunc fname RetArg, TVar xvar)]
                      ]


getPathConstraints :: [LStmt] -> (Either CheckerError [Constraint], CheckerState)
getPathConstraints = runChecker . fmap concat . mapM getStmtConstraints

-- subs any `TVar v` with `vt` in `t`
subSym :: TSym -> T -> T -> T
subSym v vt t@(TVar v')
  | v == v' = vt
  | otherwise = t
subSym v vt (TFunc argTypes returnType) = TFunc
                                          (subSym v vt <$> argTypes)
                                          (subSym v vt returnType)
subSym _ _ t = t

subLeft :: TSym -> T -> Constraint -> Constraint
subLeft v vt (cl, cr) = (subSym v vt cl, subSym v vt cr)

subRight :: TSym -> T -> Constraint -> Constraint
subRight v vt (t@(TVar _), cr) = (t, subSym v vt cr)
subRight v vt (cl, cr) = (subSym v vt cl, subSym v vt cr)

unifyConstraints_ :: [Constraint] -> [Constraint] -> [Constraint]
unifyConstraints_ [] rs = rs
unifyConstraints_ ((c@(TVar sym, b)):xs) rs =
                   unifyConstraints_ (subLeft sym b <$> xs) (c:(subRight sym b <$> rs))
unifyConstraints_ (((b, TVar sym)):xs) rs =
                   unifyConstraints_ (subLeft sym b <$> xs) ((TVar sym, b):(subRight sym b <$> rs))
unifyConstraints_ (c:cs) rs = unifyConstraints_ cs (c:rs)
