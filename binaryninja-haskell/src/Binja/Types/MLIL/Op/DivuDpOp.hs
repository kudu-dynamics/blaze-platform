module Binja.Types.MLIL.Op.DivuDpOp where

import Binja.Prelude


data DivuDpOp expr = DivuDpOp
    { _divuDpOpLeft :: expr
    , _divuDpOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
