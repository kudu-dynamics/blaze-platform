module Hinja.Types.MLIL.Op.StoreStructOp where

import Hinja.Prelude


data StoreStructOp expr = StoreStructOp
    { _storeStructOpDest :: expr
    , _storeStructOpOffset :: Int64
    , _storeStructOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
