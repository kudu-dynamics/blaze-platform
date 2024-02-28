module Binja.Types.MLIL.Op.FsqrtOp where

import Binja.Prelude


newtype FsqrtOp expr = FsqrtOp
    { _fsqrtOpSrc :: expr
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
