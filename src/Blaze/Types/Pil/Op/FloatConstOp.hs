module Blaze.Types.Pil.Op.FloatConstOp where

import Blaze.Prelude


data FloatConstOp expr = FloatConstOp
    { _floatConstOpConstant :: Double
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, FromJSON, ToJSON)

instance Hashable a => Hashable (FloatConstOp a)
