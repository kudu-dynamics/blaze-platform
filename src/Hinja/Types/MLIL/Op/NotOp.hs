module Hinja.Types.MLIL.Op.NotOp where

import Hinja.Prelude


data NotOp expr = NotOp
    { _notOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
