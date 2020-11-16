module Binja.Types.MLIL.Op.FloatConvOp where

import Binja.Prelude


data FloatConvOp expr = FloatConvOp
    { _floatConvOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (FloatConvOp a)
instance Serial m a => Serial m (FloatConvOp a)
