module Hinja.Types.MLIL.Op.FabsOp where

import Hinja.Prelude


data FabsOp expr = FabsOp
    { _fabsOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
