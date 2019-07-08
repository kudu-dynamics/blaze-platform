module Hinja.Types.MLIL.Op.SxOp where

import Hinja.Prelude


data SxOp expr = SxOp
    { _sxOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
