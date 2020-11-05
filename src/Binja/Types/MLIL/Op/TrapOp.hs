module Binja.Types.MLIL.Op.TrapOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data TrapOp expr = TrapOp
    { _trapOpVector :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (TrapOp a)
instance Hashable a => Hashable (TrapOp a)