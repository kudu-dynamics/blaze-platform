module Hinja.Types.MLIL.Op.FloatToIntOp where

import Hinja.Prelude


data FloatToIntOp expr = FloatToIntOp
    { _floatToIntOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
