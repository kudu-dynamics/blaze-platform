module Hinja.Types.MLIL.Op.NegOp where

import Hinja.Prelude


data NegOp expr = NegOp
    { _negOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
