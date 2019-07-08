module Hinja.Types.MLIL.Op.FloatConvOp where

import Hinja.Prelude


data FloatConvOp expr = FloatConvOp
    { _floatConvOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
