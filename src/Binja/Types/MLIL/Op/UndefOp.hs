module Binja.Types.MLIL.Op.UndefOp where

import Binja.Prelude


data UndefOp expr = UndefOp
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (UndefOp a)
instance Serial m a => Serial m (UndefOp a)
