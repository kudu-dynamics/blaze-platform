module Hinja.Types.MLIL.Op.UnimplOp where

import Hinja.Prelude


data UnimplOp expr = UnimplOp
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
