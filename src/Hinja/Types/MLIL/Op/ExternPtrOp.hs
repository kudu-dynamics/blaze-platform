module Hinja.Types.MLIL.Op.ExternPtrOp where

import Hinja.Prelude


data ExternPtrOp expr = ExternPtrOp
    { _externPtrOpConstant :: Int64
    , _externPtrOpOffset :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
