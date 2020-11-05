module Binja.Types.MLIL.Op.DivuDpOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data DivuDpOp expr = DivuDpOp
    { _divuDpOpLeft :: expr
    , _divuDpOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (DivuDpOp a)
instance Hashable a => Hashable (DivuDpOp a)