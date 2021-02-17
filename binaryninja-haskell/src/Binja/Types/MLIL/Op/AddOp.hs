module Binja.Types.MLIL.Op.AddOp where

import Binja.Prelude


data AddOp expr = AddOp
    { _addOpLeft :: expr
    , _addOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
