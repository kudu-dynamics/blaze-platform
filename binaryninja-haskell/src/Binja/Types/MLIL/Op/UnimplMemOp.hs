module Binja.Types.MLIL.Op.UnimplMemOp where

import Binja.Prelude


newtype UnimplMemOp expr = UnimplMemOp
    { _unimplMemOpSrc :: expr
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
