module Binja.Types.MLIL.Op.FloatToIntOp where

import Binja.Prelude


newtype FloatToIntOp expr = FloatToIntOp
    { _floatToIntOpSrc :: expr
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
