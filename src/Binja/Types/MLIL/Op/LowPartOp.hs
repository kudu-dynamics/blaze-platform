module Binja.Types.MLIL.Op.LowPartOp where

import Binja.Prelude


data LowPartOp expr = LowPartOp
    { _lowPartOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
