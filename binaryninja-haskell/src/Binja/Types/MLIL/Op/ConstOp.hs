module Binja.Types.MLIL.Op.ConstOp where

import Binja.Prelude


newtype ConstOp expr = ConstOp
    { _constOpConstant :: Int64
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
