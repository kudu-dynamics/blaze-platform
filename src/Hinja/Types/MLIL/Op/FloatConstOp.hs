module Hinja.Types.MLIL.Op.FloatConstOp where

import Hinja.Prelude


data FloatConstOp expr = FloatConstOp
    { _floatConstOpConstant :: Double
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
