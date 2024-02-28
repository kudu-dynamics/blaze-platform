module Binja.Types.MLIL.Op.LowPartOp where

import Binja.Prelude


newtype LowPartOp expr = LowPartOp
    { _lowPartOpSrc :: expr
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
