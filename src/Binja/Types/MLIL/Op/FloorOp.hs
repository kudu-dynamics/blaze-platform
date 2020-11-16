module Binja.Types.MLIL.Op.FloorOp where

import Binja.Prelude


data FloorOp expr = FloorOp
    { _floorOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (FloorOp a)
instance Hashable a => Hashable (FloorOp a)