module Binja.Types.MLIL.Op.StoreStructOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data StoreStructOp expr = StoreStructOp
    { _storeStructOpDest :: expr
    , _storeStructOpOffset :: Int64
    , _storeStructOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (StoreStructOp a)
instance Hashable a => Hashable (StoreStructOp a)