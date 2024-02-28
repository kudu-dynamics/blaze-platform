module Binja.Types.MLIL.Op.FloatConstOp where

import Binja.Prelude


newtype FloatConstOp expr = FloatConstOp
    { _floatConstOpConstant :: Double
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
