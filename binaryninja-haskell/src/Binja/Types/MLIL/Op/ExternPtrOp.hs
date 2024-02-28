module Binja.Types.MLIL.Op.ExternPtrOp where

import Binja.Prelude


data ExternPtrOp expr = ExternPtrOp
    { _externPtrOpConstant :: Int64
    , _externPtrOpOffset :: Int64
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
