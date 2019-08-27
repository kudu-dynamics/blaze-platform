module Binja.Types.MLIL.Op.IntToFloatOp where

import Binja.Prelude


data IntToFloatOp expr = IntToFloatOp
    { _intToFloatOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
