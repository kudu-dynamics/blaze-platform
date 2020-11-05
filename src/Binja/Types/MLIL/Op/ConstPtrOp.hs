module Binja.Types.MLIL.Op.ConstPtrOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data ConstPtrOp expr = ConstPtrOp
    { _constPtrOpConstant :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (ConstPtrOp a)
instance Hashable a => Hashable (ConstPtrOp a)