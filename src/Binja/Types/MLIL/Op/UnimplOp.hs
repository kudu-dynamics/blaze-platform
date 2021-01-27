module Binja.Types.MLIL.Op.UnimplOp where

import Binja.Prelude


data UnimplOp expr = UnimplOp
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (UnimplOp a)
