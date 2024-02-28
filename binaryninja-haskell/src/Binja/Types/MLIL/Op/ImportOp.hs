module Binja.Types.MLIL.Op.ImportOp where

import Binja.Prelude


newtype ImportOp expr = ImportOp
    { _importOpConstant :: Int64
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
