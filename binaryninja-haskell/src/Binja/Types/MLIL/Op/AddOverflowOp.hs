module Binja.Types.MLIL.Op.AddOverflowOp where

import Binja.Prelude


data AddOverflowOp expr = AddOverflowOp
    { _addOverflowOpLeft :: expr
    , _addOverflowOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
