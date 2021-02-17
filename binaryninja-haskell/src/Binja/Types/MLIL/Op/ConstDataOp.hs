module Binja.Types.MLIL.Op.ConstDataOp where

import Binja.Prelude


newtype ConstDataOp expr = ConstDataOp
    { _constDataOpConstant :: Int64
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
