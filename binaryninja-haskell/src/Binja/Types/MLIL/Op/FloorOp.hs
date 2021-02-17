module Binja.Types.MLIL.Op.FloorOp where

import Binja.Prelude


newtype FloorOp expr = FloorOp
    { _floorOpSrc :: expr
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
