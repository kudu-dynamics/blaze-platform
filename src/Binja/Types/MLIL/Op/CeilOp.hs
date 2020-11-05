module Binja.Types.MLIL.Op.CeilOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data CeilOp expr = CeilOp
    { _ceilOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (CeilOp a)
instance Hashable a => Hashable (CeilOp a)