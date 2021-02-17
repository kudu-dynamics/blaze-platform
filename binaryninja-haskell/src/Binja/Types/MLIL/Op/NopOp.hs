module Binja.Types.MLIL.Op.NopOp where

import Binja.Prelude


data NopOp expr = NopOp
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
