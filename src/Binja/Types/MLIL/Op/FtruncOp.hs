module Binja.Types.MLIL.Op.FtruncOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data FtruncOp expr = FtruncOp
    { _ftruncOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (FtruncOp a)
instance Hashable a => Hashable (FtruncOp a)