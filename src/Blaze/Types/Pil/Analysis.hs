{-# LANGUAGE TemplateHaskell #-}

module Blaze.Types.Pil.Analysis where

import Blaze.Prelude
import Blaze.Types.Pil
  ( Expression,
    Stmt,
  )
import qualified Blaze.Types.Pil as Pil


data MemEquivGroup
  = MemEquivGroup
      { _memEquivGroupStore :: Maybe StoreStmt,
        _memEquivGroupAddr :: MemAddr,
        -- There may be multiple LoadStmt instances for a single
        -- statement if that statement includes multiple matching 
        -- load expressions.
        _memEquivGroupLoads :: [LoadStmt]
      } deriving (Eq, Ord, Show, Generic)
instance Hashable MemEquivGroup

type Index = Word64
type MemAddr = Expression
type BitWidth = Word64
type VarName = Text

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

data MemStmt
  = MemStoreStmt StoreStmt
  | MemLoadStmt LoadStmt
  deriving (Eq, Ord, Show, Generic)
instance Hashable MemStmt

-- Need the expression in order to include the size of the value being loaded
-- from memory. NB: We don't need the same for StoreOp/Store because the size
-- of the store is inferred from the value to be written. Is that a safe assumption?
newtype LoadExpr = LoadExpr {_expr :: Expression}
                   deriving (Eq, Ord, Show, Generic)
instance Hashable LoadExpr

$(makeFields ''StoreStmt)
$(makeFields ''LoadStmt)
$(makeFields ''MemEquivGroup)
$(makeFields ''MemStorage)