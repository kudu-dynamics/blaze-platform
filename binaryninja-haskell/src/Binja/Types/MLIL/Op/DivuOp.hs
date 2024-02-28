module Binja.Types.MLIL.Op.DivuOp where

import Binja.Prelude


data DivuOp expr = DivuOp
    { _divuOpLeft :: expr
    , _divuOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
