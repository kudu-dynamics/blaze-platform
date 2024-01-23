module Binja.Types.MLIL.Op.FloorOp where

import Binja.Prelude


data FloorOp expr = FloorOp
    { _floorOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (FloorOp a)
