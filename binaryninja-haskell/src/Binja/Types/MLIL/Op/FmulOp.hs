module Binja.Types.MLIL.Op.FmulOp where

import Binja.Prelude


data FmulOp expr = FmulOp
    { _fmulOpLeft :: expr
    , _fmulOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
