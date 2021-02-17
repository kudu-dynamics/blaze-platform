module Binja.Types.MLIL.Op.SubOp where

import Binja.Prelude


data SubOp expr = SubOp
    { _subOpLeft :: expr
    , _subOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
