module Hinja.Types.MLIL.Op.ConstPtrOp where

import Hinja.Prelude


data ConstPtrOp expr = ConstPtrOp
    { _constPtrOpConstant :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
