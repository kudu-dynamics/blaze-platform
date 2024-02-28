module Binja.Types.MLIL.Op.UndefOp where

import Binja.Prelude


data UndefOp expr = UndefOp
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
