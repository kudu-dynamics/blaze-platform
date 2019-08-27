module Binja.Types.MLIL.Op.FloatConvOp where

import Binja.Prelude


data FloatConvOp expr = FloatConvOp
    { _floatConvOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
