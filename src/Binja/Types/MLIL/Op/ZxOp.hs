module Binja.Types.MLIL.Op.ZxOp where

import Binja.Prelude


data ZxOp expr = ZxOp
    { _zxOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
