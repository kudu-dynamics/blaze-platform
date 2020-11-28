module Binja.Types.MLIL.Op.FtruncOp where

import Binja.Prelude


data FtruncOp expr = FtruncOp
    { _ftruncOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (FtruncOp a)
instance Serial m a => Serial m (FtruncOp a)
