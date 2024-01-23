module Binja.Types.MLIL.Op.FloatToIntOp where

import Binja.Prelude


data FloatToIntOp expr = FloatToIntOp
    { _floatToIntOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (FloatToIntOp a)
