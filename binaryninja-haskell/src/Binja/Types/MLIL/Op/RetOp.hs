module Binja.Types.MLIL.Op.RetOp where

import Binja.Prelude


newtype RetOp expr = RetOp
    { _retOpSrc :: [expr]
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
