module Binja.Types.MLIL.Op.FtruncOp where

import Binja.Prelude


newtype FtruncOp expr = FtruncOp
    { _ftruncOpSrc :: expr
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
