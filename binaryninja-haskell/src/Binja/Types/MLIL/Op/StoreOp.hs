module Binja.Types.MLIL.Op.StoreOp where

import Binja.Prelude


data StoreOp expr = StoreOp
    { _storeOpDest :: expr
    , _storeOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (StoreOp a)
