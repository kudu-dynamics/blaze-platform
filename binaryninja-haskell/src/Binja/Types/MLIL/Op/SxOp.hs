module Binja.Types.MLIL.Op.SxOp where

import Binja.Prelude


newtype SxOp expr = SxOp
    { _sxOpSrc :: expr
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
