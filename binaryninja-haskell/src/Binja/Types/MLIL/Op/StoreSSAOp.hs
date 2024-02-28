module Binja.Types.MLIL.Op.StoreSSAOp where

import Binja.Prelude


data StoreSSAOp expr = StoreSSAOp
    { _storeSSAOpDest :: expr
    , _storeSSAOpDest_memory :: Int64
    , _storeSSAOpSrc_memory :: Int64
    , _storeSSAOpSrc :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
