module Binja.Types.MLIL.Op.ModsOp where

import Binja.Prelude


data ModsOp expr = ModsOp
    { _modsOpLeft :: expr
    , _modsOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
