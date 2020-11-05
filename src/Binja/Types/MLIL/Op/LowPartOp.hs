module Binja.Types.MLIL.Op.LowPartOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data LowPartOp expr = LowPartOp
    { _lowPartOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (LowPartOp a)
instance Hashable a => Hashable (LowPartOp a)