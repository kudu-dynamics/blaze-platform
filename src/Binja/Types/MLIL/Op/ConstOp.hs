module Binja.Types.MLIL.Op.ConstOp where

import Binja.Prelude


data ConstOp expr = ConstOp
    { _constOpConstant :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
