module Binja.Types.MLIL.Op.DivsDpOp where

import Binja.Prelude


data DivsDpOp expr = DivsDpOp
    { _divsDpOpLeft :: expr
    , _divsDpOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
