module Hinja.Types.MLIL.Op.DivuOp where

import Hinja.Prelude


data DivuOp expr = DivuOp
    { _divuOpLeft :: expr
    , _divuOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
