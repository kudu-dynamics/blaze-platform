module Hinja.Types.MLIL.Op.FtruncOp where

import Hinja.Prelude


data FtruncOp expr = FtruncOp
    { _ftruncOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
