module Binja.Types.MLIL.Op.NoretOp where

import Binja.Prelude


data NoretOp expr = NoretOp
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
