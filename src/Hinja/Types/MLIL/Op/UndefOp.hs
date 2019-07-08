module Hinja.Types.MLIL.Op.UndefOp where

import Hinja.Prelude


data UndefOp expr = UndefOp
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
