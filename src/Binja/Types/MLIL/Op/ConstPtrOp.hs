module Binja.Types.MLIL.Op.ConstPtrOp where

import Binja.Prelude


data ConstPtrOp expr = ConstPtrOp
    { _constPtrOpConstant :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
