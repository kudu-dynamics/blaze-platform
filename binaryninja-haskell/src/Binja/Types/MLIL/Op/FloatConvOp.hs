module Binja.Types.MLIL.Op.FloatConvOp where

import Binja.Prelude


newtype FloatConvOp expr = FloatConvOp
    { _floatConvOpSrc :: expr
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
