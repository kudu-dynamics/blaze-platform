module Binja.Types.MLIL.Op.StoreSSAOp where

import Binja.Prelude


data StoreSSAOp expr = StoreSSAOp
    { _storeSSAOpDest :: expr
    , _storeSSAOpDest_memory :: Int64
    , _storeSSAOpSrc_memory :: Int64
    , _storeSSAOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (StoreSSAOp a)
instance Hashable a => Hashable (StoreSSAOp a)