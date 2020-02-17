{-# LANGUAGE TemplateHaskell #-}

module Blaze.Types.Pil.Analysis where

import Blaze.Prelude
import Blaze.Types.Pil
  ( Expression,
    Stmt,
  )
import qualified Blaze.Types.Pil as Pil

import Data.HashMap.Strict (HashMap)


data MemEquivGroup
  = MemEquivGroup
      { _memEquivGroupStore :: Maybe StoreStmt,
        _memEquivGroupAddr :: MemAddr,
        -- There may be multiple LoadStmt instances for a single
        -- statement if that statement includes multiple matching 
        -- load expressions.
        _memEquivGroupLoads :: [LoadStmt]
      } deriving (Eq, Ord, Show, Generic)

data MemEquivGroupState
  = MemEquivGroupState
      { allGroups :: [MemEquivGroup],
        liveGroups :: HashMap Expression MemEquivGroup
      } deriving (Eq, Ord, Show, Generic)

-- TODO: Consider adding this to Blaze.Pil as a LoadOp
--       Is anything other than LoadSSA used for MLIL SSA?
--       I think we can specialize this version of PIL (or PIL*?) for MLIL SSA
-- data Load expr
--   = Load expr
--   | LoadSSA expr
--   | LoadStruct expr
--   | LoadStructSSA expr
--   deriving (Show, Eq)

type Index = Int
type MemAddr = Expression
type BitWidth = Int

data Storage
  = Storage
      { start :: MemAddr,
        size :: BitWidth
      } deriving (Eq, Ord, Show, Generic)
instance Hashable Storage

data StoreStmt
  = StoreStmt
      { _storeStmtStmt :: Stmt,
        _storeStmtOp :: Pil.StoreOp Expression,
        _storeStmtAddr :: MemAddr
      } deriving (Eq, Ord, Show, Generic)
instance Hashable StoreStmt

data LoadStmt
  = LoadStmt
      { _loadStmtStmt :: Stmt,
        _loadStmtLoadExpr :: LoadExpr,
        _loadStmtAddr :: MemAddr
      } deriving (Eq, Ord, Show, Generic)
instance Hashable LoadStmt

data MemStmt
  = MemStoreStmt StoreStmt
  | MemLoadStmt LoadStmt

-- Need the expression in order to include the size of the value being loaded
-- from memory. NB: We don't need the same for StoreOp/Store because the size
-- of the store is inferred from the value to be written. Is that a safe assumption?
newtype LoadExpr = LoadExpr {_expr :: Expression}
                   deriving (Eq, Ord, Show, Generic)
instance Hashable LoadExpr

$(makeFields ''StoreStmt)
$(makeFields ''LoadStmt)
$(makeFields ''MemEquivGroup)