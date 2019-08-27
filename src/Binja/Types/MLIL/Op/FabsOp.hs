module Binja.Types.MLIL.Op.FabsOp where

import Binja.Prelude


data FabsOp expr = FabsOp
    { _fabsOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
