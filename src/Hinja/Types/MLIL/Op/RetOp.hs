module Hinja.Types.MLIL.Op.RetOp where

import Hinja.Prelude


data RetOp expr = RetOp
    { _retOpSrc :: [expr]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
