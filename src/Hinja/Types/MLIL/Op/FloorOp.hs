module Hinja.Types.MLIL.Op.FloorOp where

import Hinja.Prelude


data FloorOp expr = FloorOp
    { _floorOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
