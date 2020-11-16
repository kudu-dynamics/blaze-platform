module Binja.Types.MLIL.Op.UndefOp where

import Binja.Prelude


data UndefOp expr = UndefOp
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (UndefOp a)
instance Hashable a => Hashable (UndefOp a)