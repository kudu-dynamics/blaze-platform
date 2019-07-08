module Hinja.Types.MLIL.Op.UnimplMemOp where

import Hinja.Prelude


data UnimplMemOp expr = UnimplMemOp
    { _unimplMemOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
