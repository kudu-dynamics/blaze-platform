module Binja.Types.MLIL.Op.StoreOp where

import Binja.Prelude


data StoreOp expr = StoreOp
    { _storeOpDest :: expr
    , _storeOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (StoreOp a)
instance Hashable a => Hashable (StoreOp a)