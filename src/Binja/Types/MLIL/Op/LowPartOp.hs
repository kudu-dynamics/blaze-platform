module Binja.Types.MLIL.Op.LowPartOp where

import Binja.Prelude


data LowPartOp expr = LowPartOp
    { _lowPartOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (LowPartOp a)
instance Serial m a => Serial m (LowPartOp a)