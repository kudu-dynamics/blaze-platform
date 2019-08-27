module Binja.Types.MLIL.Op.CeilOp where

import Binja.Prelude


data CeilOp expr = CeilOp
    { _ceilOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
