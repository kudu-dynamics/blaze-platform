module Binja.Types.MLIL.Op.FloatConvOp where

import Binja.Prelude


data FloatConvOp expr = FloatConvOp
    { _floatConvOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (FloatConvOp a)
instance Hashable a => Hashable (FloatConvOp a)