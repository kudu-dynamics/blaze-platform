module Hinja.Types.MLIL.Op.NoretOp where

import Hinja.Prelude


data NoretOp expr = NoretOp
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
