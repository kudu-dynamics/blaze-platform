module Binja.Types.MLIL.Op.RoundToIntOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data RoundToIntOp expr = RoundToIntOp
    { _roundToIntOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (RoundToIntOp a)
instance Hashable a => Hashable (RoundToIntOp a)