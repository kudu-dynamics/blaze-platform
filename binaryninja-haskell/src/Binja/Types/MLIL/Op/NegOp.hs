module Binja.Types.MLIL.Op.NegOp where

import Binja.Prelude


newtype NegOp expr = NegOp
    { _negOpSrc :: expr
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
