module Binja.Types.MLIL.Op.MulsDpOp where

import Binja.Prelude


data MulsDpOp expr = MulsDpOp
    { _mulsDpOpLeft :: expr
    , _mulsDpOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
