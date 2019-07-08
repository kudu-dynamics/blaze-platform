module Hinja.Types.MLIL.Op.NopOp where

import Hinja.Prelude


data NopOp expr = NopOp
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
