module Binja.Types.MLIL.Op.BpOp where

import Binja.Prelude


data BpOp expr = BpOp
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (BpOp a)
instance Hashable a => Hashable (BpOp a)