module Binja.Types.MLIL.Op.NotOp where

import Binja.Prelude


newtype NotOp expr = NotOp
    { _notOpSrc :: expr
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
