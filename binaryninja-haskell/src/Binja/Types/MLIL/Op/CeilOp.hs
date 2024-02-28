module Binja.Types.MLIL.Op.CeilOp where

import Binja.Prelude


newtype CeilOp expr = CeilOp
    { _ceilOpSrc :: expr
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
