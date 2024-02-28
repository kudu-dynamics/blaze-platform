module Binja.Types.MLIL.Op.MulOp where

import Binja.Prelude


data MulOp expr = MulOp
    { _mulOpLeft :: expr
    , _mulOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
