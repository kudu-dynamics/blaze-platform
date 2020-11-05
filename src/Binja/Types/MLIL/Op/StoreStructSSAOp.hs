module Binja.Types.MLIL.Op.StoreStructSSAOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data StoreStructSSAOp expr = StoreStructSSAOp
    { _storeStructSSAOpDest :: expr
    , _storeStructSSAOpOffset :: Int64
    , _storeStructSSAOpDest_memory :: Int64
    , _storeStructSSAOpSrc_memory :: Int64
    , _storeStructSSAOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (StoreStructSSAOp a)
instance Hashable a => Hashable (StoreStructSSAOp a)