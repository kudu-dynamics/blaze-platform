module Binja.Types.MLIL.Op.BpOp where

import Binja.Prelude


data BpOp expr = BpOp
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
