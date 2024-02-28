module Binja.Types.MLIL.Op.FdivOp where

import Binja.Prelude


data FdivOp expr = FdivOp
    { _fdivOpLeft :: expr
    , _fdivOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
