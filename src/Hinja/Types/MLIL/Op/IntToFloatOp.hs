module Hinja.Types.MLIL.Op.IntToFloatOp where

import Hinja.Prelude


data IntToFloatOp expr = IntToFloatOp
    { _intToFloatOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
