module Binja.Types.MLIL.Op.BoolToIntOp where

import Binja.Prelude


newtype BoolToIntOp expr = BoolToIntOp
    { _boolToIntOpSrc :: expr
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
