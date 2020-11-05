module Binja.Types.MLIL.Op.NotOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data NotOp expr = NotOp
    { _notOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (NotOp a)
instance Hashable a => Hashable (NotOp a)