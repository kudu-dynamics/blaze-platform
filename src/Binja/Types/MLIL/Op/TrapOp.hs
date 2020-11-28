module Binja.Types.MLIL.Op.TrapOp where

import Binja.Prelude


data TrapOp expr = TrapOp
    { _trapOpVector :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (TrapOp a)
instance Serial m a => Serial m (TrapOp a)
