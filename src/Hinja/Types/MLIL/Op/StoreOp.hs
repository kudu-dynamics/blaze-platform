module Hinja.Types.MLIL.Op.StoreOp where

import Hinja.Prelude


data StoreOp expr = StoreOp
    { _storeOpDest :: expr
    , _storeOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
