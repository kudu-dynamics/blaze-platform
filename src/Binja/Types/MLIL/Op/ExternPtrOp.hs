module Binja.Types.MLIL.Op.ExternPtrOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data ExternPtrOp expr = ExternPtrOp
    { _externPtrOpConstant :: Int64
    , _externPtrOpOffset :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (ExternPtrOp a)
instance Hashable a => Hashable (ExternPtrOp a)