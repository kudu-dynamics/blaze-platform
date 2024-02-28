module Binja.Types.MLIL.Op.XorOp where

import Binja.Prelude


data XorOp expr = XorOp
    { _xorOpLeft :: expr
    , _xorOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
