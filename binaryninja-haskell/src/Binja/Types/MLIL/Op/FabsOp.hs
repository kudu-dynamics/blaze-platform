module Binja.Types.MLIL.Op.FabsOp where

import Binja.Prelude


newtype FabsOp expr = FabsOp
    { _fabsOpSrc :: expr
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
