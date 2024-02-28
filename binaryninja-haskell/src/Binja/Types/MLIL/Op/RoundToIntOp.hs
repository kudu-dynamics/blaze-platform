module Binja.Types.MLIL.Op.RoundToIntOp where

import Binja.Prelude


newtype RoundToIntOp expr = RoundToIntOp
    { _roundToIntOpSrc :: expr
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
