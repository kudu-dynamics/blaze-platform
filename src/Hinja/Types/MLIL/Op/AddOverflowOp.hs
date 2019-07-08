module Hinja.Types.MLIL.Op.AddOverflowOp where

import Hinja.Prelude


data AddOverflowOp expr = AddOverflowOp
    { _addOverflowOpLeft :: expr
    , _addOverflowOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
