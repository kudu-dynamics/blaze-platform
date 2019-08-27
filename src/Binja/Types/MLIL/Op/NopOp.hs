module Binja.Types.MLIL.Op.NopOp where

import Binja.Prelude


data NopOp expr = NopOp
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
