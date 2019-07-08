module Hinja.Types.MLIL.Op.LoadOp where

import Hinja.Prelude


data LoadOp expr = LoadOp
    { _loadOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
