module Binja.Types.MLIL.Op.AndOp where

import Binja.Prelude


data AndOp expr = AndOp
    { _andOpLeft :: expr
    , _andOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
