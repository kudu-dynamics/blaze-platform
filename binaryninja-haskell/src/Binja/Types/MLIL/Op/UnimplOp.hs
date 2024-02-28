module Binja.Types.MLIL.Op.UnimplOp where

import Binja.Prelude


data UnimplOp expr = UnimplOp
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
