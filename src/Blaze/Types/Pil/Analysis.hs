{-# LANGUAGE TemplateHaskell #-}

module Blaze.Types.Pil.Analysis where

import Blaze.Prelude hiding (Symbol)
import Blaze.Types.Pil
  ( Expression,
    Stmt,
    Symbol,
    PilVar
  )
import qualified Blaze.Types.Pil as Pil
import Data.Digits (digits)
import qualified Data.Text as Text
import qualified Data.HashSet as HSet

data MemEquivGroup
  = MemEquivGroup
      { _memEquivGroupStore :: Maybe StoreStmt,
        _memEquivGroupStorage :: MemStorage,
        -- There may be multiple LoadStmt instances for a single
        -- statement if that statement includes multiple matching 
        -- load expressions.
        -- def x [load]
        _memEquivGroupDefLoads :: [DefLoadStmt],

        -- any stmt with nested load
        -- if there are n loads in a single stmt, there will be n LoadStmts
        _memEquivGroupNestedLoads :: [LoadStmt]
      } deriving (Eq, Ord, Show, Generic)
instance Hashable MemEquivGroup

-- TODO: Index should really be unsigned, but Haskell idiom
--       seems to be to just always use signed ints
type Index = Int
type MemAddr = Expression
type BitWidth = Word64

data MemStorage
  = MemStorage
      { _memStorageStart :: MemAddr,
        _memStorageWidth :: BitWidth
      } deriving (Eq, Ord, Show, Generic)
instance Hashable MemStorage

data StoreStmt
  = StoreStmt
      { _storeStmtStmt :: Stmt,
        _storeStmtOp :: Pil.StoreOp Expression,
        _storeStmtStorage :: MemStorage,
        _storeStmtIndex :: Index
      } deriving (Eq, Ord, Show, Generic)
instance Hashable StoreStmt

data LoadStmt
  = LoadStmt
      { _loadStmtStmt :: Stmt,
        _loadStmtLoadExpr :: LoadExpr,
        _loadStmtStorage :: MemStorage,
        _loadStmtIndex :: Index
      } deriving (Eq, Ord, Show, Generic)
instance Hashable LoadStmt

data DefLoadStmt
  = DefLoadStmt
    { _defLoadStmtVar :: PilVar,
      _defLoadStmtLoadStmt :: LoadStmt
    } deriving (Eq, Ord, Show, Generic)
instance Hashable DefLoadStmt

data MemStmt
  = MemStoreStmt StoreStmt
  | MemNestedLoadStmt LoadStmt
  | MemDefLoadStmt DefLoadStmt
  deriving (Eq, Ord, Show, Generic)
instance Hashable MemStmt


-- Need the expression in order to include the size of the value being loaded
-- from memory. NB: We don't need the same for StoreOp/Store because the size
-- of the store is inferred from the value to be written. Is that a safe assumption?
newtype LoadExpr
  = LoadExpr { _loadExprExpr :: Expression }
  deriving (Eq, Ord, Show, Generic)

instance Hashable LoadExpr

newtype ConstLoadExpr
  = ConstLoadExpr Expression
  deriving (Eq, Ord, Show, Generic)

instance Hashable ConstLoadExpr

-- this doesn't work quite as expected.
-- since "a" is zero and "b" is 1, you never get "aa"
symNumToSymbol :: Word64 -> Symbol
symNumToSymbol 0 = "a"
symNumToSymbol n = Text.pack
  . fmap (chr . (+97) . fromIntegral)
  . digits 26 $ n

-- TODO: Make this better.
-- |Generate variable names.
symbolGenerator :: HashSet Symbol -> [Symbol]
symbolGenerator usedNames = [x | x <- names, not $ HSet.member x usedNames]
  where
    letters :: String
    letters = ['a'..'z']

    names :: [Symbol]
    names = [Text.pack [a, b, c] | a <- letters, 
                                   b <- letters,
                                   c <- letters]

data AnalysisState = AnalysisState
                   { _analysisStateNewSymbols :: [Symbol]
                   } deriving (Eq, Ord, Read, Show)

$(makeFields ''AnalysisState)

newtype Analysis a = Analysis { _runAnalysis :: State AnalysisState a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState AnalysisState
           )

newSym :: Analysis Symbol
newSym = do
  slist <- use newSymbols
  newSymbols %= drop 1
  return . maybe "RAN_OUT_OF_SYMBOLS" identity . headMay $ slist
  

runAnalysis :: Analysis a -> HashSet Symbol -> a
runAnalysis m usedSymbols = flip evalState s . _runAnalysis $ m
  where
    s = AnalysisState
      { _analysisStateNewSymbols = symbolGenerator usedSymbols
      }


$(makeFields ''StoreStmt)
$(makeFields ''LoadStmt)
$(makeFields ''DefLoadStmt)
$(makeFields ''MemEquivGroup)
$(makeFields ''MemStorage)
$(makeFields ''LoadExpr)
