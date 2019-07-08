module Hinja.Types.MLIL.Op.LowPartOp where

import Hinja.Prelude


data LowPartOp expr = LowPartOp
    { _lowPartOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
