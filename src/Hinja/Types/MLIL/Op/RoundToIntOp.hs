module Hinja.Types.MLIL.Op.RoundToIntOp where

import Hinja.Prelude


data RoundToIntOp expr = RoundToIntOp
    { _roundToIntOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
