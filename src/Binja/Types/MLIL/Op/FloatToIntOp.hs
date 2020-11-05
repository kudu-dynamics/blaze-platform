module Binja.Types.MLIL.Op.FloatToIntOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data FloatToIntOp expr = FloatToIntOp
    { _floatToIntOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (FloatToIntOp a)
instance Hashable a => Hashable (FloatToIntOp a)