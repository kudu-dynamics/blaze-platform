module Hinja.Types.MLIL.Op.MulsDpOp where

import Hinja.Prelude


data MulsDpOp expr = MulsDpOp
    { _mulsDpOpLeft :: expr
    , _mulsDpOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
