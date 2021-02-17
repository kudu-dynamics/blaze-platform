module Binja.Types.MLIL.Op.ZxOp where

import Binja.Prelude


newtype ZxOp expr = ZxOp
    { _zxOpSrc :: expr
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
