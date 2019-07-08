module Hinja.Types.MLIL.Op.BoolToIntOp where

import Hinja.Prelude


data BoolToIntOp expr = BoolToIntOp
    { _boolToIntOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
