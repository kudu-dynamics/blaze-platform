module Binja.Types.MLIL.Op.NopOp where

import Binja.Prelude


data NopOp expr = NopOp
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (NopOp a)
instance Serial m a => Serial m (NopOp a)