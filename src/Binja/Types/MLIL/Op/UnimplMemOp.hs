module Binja.Types.MLIL.Op.UnimplMemOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data UnimplMemOp expr = UnimplMemOp
    { _unimplMemOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (UnimplMemOp a)
instance Hashable a => Hashable (UnimplMemOp a)