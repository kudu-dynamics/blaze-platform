module Binja.Types.MLIL.Op.ConstPtrOp where

import Binja.Prelude


newtype ConstPtrOp expr = ConstPtrOp
    { _constPtrOpConstant :: Int64
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
