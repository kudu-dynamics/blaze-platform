module Binja.Types.MLIL.Op.FabsOp where

import Binja.Prelude


data FabsOp expr = FabsOp
    { _fabsOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (FabsOp a)
instance Hashable a => Hashable (FabsOp a)