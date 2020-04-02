{-# LANGUAGE TemplateHaskell #-}

module Blaze.Types.Pil.Analysis where

import Blaze.Prelude
import Blaze.Types.Pil
  ( Expression,
    Stmt,
  )
import qualified Blaze.Types.Pil as Pil

import Prelude (show)
import Text.Printf (printf)

-- |A concrete address.
newtype Address = Address Word64
  deriving (Eq, Ord, Num, Real, Enum, Integral, Generic)
instance Hashable Address

instance Show Address where
  show (Address x) = printf "Address 0x%x" x

data MemEquivGroup
  = MemEquivGroup
      { _memEquivGroupStore :: Maybe StoreStmt,
        _memEquivGroupStorage :: MemStorage,
        -- There may be multiple LoadStmt instances for a single
        -- statement if that statement includes multiple matching 
        -- load expressions.
        _memEquivGroupLoads :: [LoadStmt]
      } deriving (Eq, Ord, Show, Generic)
instance Hashable MemEquivGroup

-- TODO: Index should really be unsigned, but Haskell idiom
--       seems to be to just always use signed ints
type Index = Int
type MemAddr = Expression
--type BitWidth = Word64

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
newtype LoadExpr
  = LoadExpr { _loadExprExpr :: Expression }
  deriving (Eq, Ord, Show, Generic)

instance Hashable LoadExpr

newtype ConstLoadExpr
  = ConstLoadExpr Expression
  deriving (Eq, Ord, Show, Generic)

instance Hashable ConstLoadExpr


$(makeFields ''StoreStmt)
$(makeFields ''LoadStmt)
$(makeFields ''MemEquivGroup)
$(makeFields ''MemStorage)
$(makeFields ''LoadExpr)
