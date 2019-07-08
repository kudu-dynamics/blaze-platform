module Hinja.Types.MLIL.Op.TrapOp where

import Hinja.Prelude


data TrapOp expr = TrapOp
    { _trapOpVector :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
