module Binja.Types.MLIL.Op.LoadOp where

import Binja.Prelude


newtype LoadOp expr = LoadOp
    { _loadOpSrc :: expr
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
