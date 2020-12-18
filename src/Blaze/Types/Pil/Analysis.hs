{-# LANGUAGE TemplateHaskell #-}

module Blaze.Types.Pil.Analysis where

import Blaze.Prelude hiding (Symbol)
import Blaze.Types.Pil
  ( Expression,
    PilVar,
    Stmt,
    Symbol,
  )
import qualified Blaze.Types.Pil as Pil

import qualified Data.HashSet as HSet

import qualified Data.Text as Text

data MemEquivGroup = MemEquivGroup
  { store :: Maybe StoreStmt
  , storage :: MemStorage
    -- There may be multiple LoadStmt instances for a single
    -- statement if that statement includes multiple matching
    -- load expressions.
    -- E.g.: def x [load]
  , defLoads :: [DefLoadStmt]
    -- Any stmt with nested load, excluding Def-Load statements, but
    -- including store statements.
    -- If there are n loads in a single stmt, there will be n LoadStmts.
  , loads :: [LoadStmt]
  }
  deriving (Eq, Ord, Show, Generic)

instance Hashable MemEquivGroup

-- TODO: Index should really be unsigned, but Haskell idiom
--       seems to be to just always use signed ints.
type Index = Int

type MemAddr = Expression

data MemStorage = MemStorage
  { start :: MemAddr
  , width :: Bits
  }
  deriving (Eq, Ord, Show, Generic)

instance Hashable MemStorage

data StoreStmt = StoreStmt
  { stmt :: Stmt
  , op :: Pil.StoreOp Expression
  , storage :: MemStorage
  , index :: Index
  }
  deriving (Eq, Ord, Show, Generic)

instance Hashable StoreStmt

data LoadStmt = LoadStmt
  { stmt :: Stmt
  , loadExpr :: LoadExpr
  , storage :: MemStorage
  , index :: Index
  }
  deriving (Eq, Ord, Show, Generic)

instance Hashable LoadStmt

data DefLoadStmt = DefLoadStmt
  { var :: PilVar
  , loadStmt :: LoadStmt
  }
  deriving (Eq, Ord, Show, Generic)

instance Hashable DefLoadStmt

data MemStmt
  = MemStoreStmt StoreStmt
  -- MemLoadStmt is any non-Def-Load statement containing a load
  | MemLoadStmt LoadStmt
  | MemDefLoadStmt DefLoadStmt
  deriving (Eq, Ord, Show, Generic)

instance Hashable MemStmt

-- Need the expression in order to include the size of the value being loaded
-- from memory. NB: We don't need the same for StoreOp/Store because the size
-- of the store is inferred from the value to be written. Is that a safe assumption?
newtype LoadExpr = LoadExpr {expr :: Expression}
  deriving (Eq, Ord, Show, Generic)

instance Hashable LoadExpr

newtype ConstLoadExpr
  = ConstLoadExpr Expression
  deriving (Eq, Ord, Show, Generic)

instance Hashable ConstLoadExpr

type EqMap a = HashMap a a

data AnalysisState = AnalysisState
  { newSymbols :: [Symbol]
  , varEqMap :: Maybe (EqMap PilVar) -- putVarEqMap
  , originMap :: Maybe (EqMap PilVar) --putOriginMap
  , fieldBaseAddrs :: HashSet Expression
  , arrayBaseAddrs :: HashSet Expression
  }
  deriving (Eq, Ord, Show, Generic)

emptyAnalysisState :: AnalysisState
emptyAnalysisState = AnalysisState
  []
  Nothing
  Nothing
  HSet.empty
  HSet.empty

newtype Analysis a = Analysis {_runAnalysis :: State AnalysisState a}
  deriving (Functor)
  deriving newtype
    ( Applicative,
      Monad,
      MonadState AnalysisState
    )

newSym :: Analysis Symbol
newSym = do
  slist <- use #newSymbols
  #newSymbols %= drop 1
  return . maybe "RAN_OUT_OF_SYMBOLS" identity . headMay $ slist

runAnalysisWithState :: Analysis a -> AnalysisState -> (a, AnalysisState)
runAnalysisWithState m s =
  flip runState s . _runAnalysis $ m

runAnalysis :: Analysis a -> HashSet Symbol -> a
runAnalysis m usedSymbols = flip evalState s . _runAnalysis $ m
  where
    s = emptyAnalysisState
        & #newSymbols .~ symbolGenerator usedSymbols 

runAnalysis_ :: Analysis a -> a
runAnalysis_ = flip evalState s . _runAnalysis
  where
    s = emptyAnalysisState



-- TODO: Make this better.
-- | Generate variable names.
symbolGenerator :: HashSet Symbol -> [Symbol]
symbolGenerator usedNames = [x | x <- names, not $ HSet.member x usedNames]
  where
    letters :: String
    letters = ['a' .. 'z']
    names :: [Symbol]
    names =
      [ Text.pack [a, b, c] | a <- letters, b <- letters, c <- letters
      ]

