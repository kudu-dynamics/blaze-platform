module Binja.Types.MLIL.Op.LsrOp where

import Binja.Prelude


data LsrOp expr = LsrOp
    { _lsrOpLeft :: expr
    , _lsrOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
