module Hinja.Types.MLIL.Op.StoreSSAOp where

import Hinja.Prelude


data StoreSSAOp expr = StoreSSAOp
    { _storeSSAOpDest :: expr
    , _storeSSAOpDest_memory :: Int64
    , _storeSSAOpSrc_memory :: Int64
    , _storeSSAOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
