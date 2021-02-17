module Binja.Types.MLIL.Op.FaddOp where

import Binja.Prelude


data FaddOp expr = FaddOp
    { _faddOpLeft :: expr
    , _faddOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
