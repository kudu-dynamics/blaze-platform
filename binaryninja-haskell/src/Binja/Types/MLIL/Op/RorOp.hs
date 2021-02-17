module Binja.Types.MLIL.Op.RorOp where

import Binja.Prelude


data RorOp expr = RorOp
    { _rorOpLeft :: expr
    , _rorOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
