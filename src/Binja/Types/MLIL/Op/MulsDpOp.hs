module Binja.Types.MLIL.Op.MulsDpOp where

import Binja.Prelude


data MulsDpOp expr = MulsDpOp
    { _mulsDpOpLeft :: expr
    , _mulsDpOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (MulsDpOp a)
instance Serial m a => Serial m (MulsDpOp a)
