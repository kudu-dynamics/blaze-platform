module Binja.Types.MLIL.Op.FloatConstOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data FloatConstOp expr = FloatConstOp
    { _floatConstOpConstant :: Double
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (FloatConstOp a)
instance Hashable a => Hashable (FloatConstOp a)