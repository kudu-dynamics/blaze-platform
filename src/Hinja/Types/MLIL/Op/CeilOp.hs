module Hinja.Types.MLIL.Op.CeilOp where

import Hinja.Prelude


data CeilOp expr = CeilOp
    { _ceilOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
