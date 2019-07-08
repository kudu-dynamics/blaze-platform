module Hinja.Types.MLIL.Op.StoreStructSSAOp where

import Hinja.Prelude


data StoreStructSSAOp expr = StoreStructSSAOp
    { _storeStructSSAOpDest :: expr
    , _storeStructSSAOpOffset :: Int64
    , _storeStructSSAOpDest_memory :: Int64
    , _storeStructSSAOpSrc_memory :: Int64
    , _storeStructSSAOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
