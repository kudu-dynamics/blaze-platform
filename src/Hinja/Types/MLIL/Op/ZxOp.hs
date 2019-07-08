module Hinja.Types.MLIL.Op.ZxOp where

import Hinja.Prelude


data ZxOp expr = ZxOp
    { _zxOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
