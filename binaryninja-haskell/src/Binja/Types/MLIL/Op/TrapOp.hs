module Binja.Types.MLIL.Op.TrapOp where

import Binja.Prelude


newtype TrapOp expr = TrapOp
    { _trapOpVector :: Int64
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
