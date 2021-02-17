module Binja.Types.MLIL.Op.IntToFloatOp where

import Binja.Prelude


newtype IntToFloatOp expr = IntToFloatOp
    { _intToFloatOpSrc :: expr
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
