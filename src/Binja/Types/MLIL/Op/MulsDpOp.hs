module Binja.Types.MLIL.Op.MulsDpOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data MulsDpOp expr = MulsDpOp
    { _mulsDpOpLeft :: expr
    , _mulsDpOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (MulsDpOp a)
instance Hashable a => Hashable (MulsDpOp a)