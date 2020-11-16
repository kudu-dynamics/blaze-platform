module Binja.Types.MLIL.Op.NotOp where

import Binja.Prelude


data NotOp expr = NotOp
    { _notOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (NotOp a)
instance Serial m a => Serial m (NotOp a)