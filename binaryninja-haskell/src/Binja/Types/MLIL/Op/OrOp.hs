module Binja.Types.MLIL.Op.OrOp where

import Binja.Prelude


data OrOp expr = OrOp
    { _orOpLeft :: expr
    , _orOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
