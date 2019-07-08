module Hinja.Types.MLIL.Op.DivuDpOp where

import Hinja.Prelude


data DivuDpOp expr = DivuDpOp
    { _divuDpOpLeft :: expr
    , _divuDpOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
