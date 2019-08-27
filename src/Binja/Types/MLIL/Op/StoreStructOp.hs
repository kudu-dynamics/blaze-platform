module Binja.Types.MLIL.Op.StoreStructOp where

import Binja.Prelude


data StoreStructOp expr = StoreStructOp
    { _storeStructOpDest :: expr
    , _storeStructOpOffset :: Int64
    , _storeStructOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
