module Binja.Types.MLIL.Op.LoadOp where

import Binja.Prelude


data LoadOp expr = LoadOp
    { _loadOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (LoadOp a)
instance Hashable a => Hashable (LoadOp a)