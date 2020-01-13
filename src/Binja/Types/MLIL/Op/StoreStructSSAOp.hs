module Binja.Types.MLIL.Op.StoreStructSSAOp where

import Binja.Prelude


data StoreStructSSAOp expr = StoreStructSSAOp
    { _storeStructSSAOpDest :: expr
    , _storeStructSSAOpOffset :: Int64
    , _storeStructSSAOpDest_memory :: Int64
    , _storeStructSSAOpSrc_memory :: Int64
    , _storeStructSSAOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (StoreStructSSAOp a)