module Binja.Types.MLIL.Op.ModuOp where

import Binja.Prelude


data ModuOp expr = ModuOp
    { _moduOpLeft :: expr
    , _moduOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
