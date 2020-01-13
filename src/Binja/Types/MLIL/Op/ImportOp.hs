module Binja.Types.MLIL.Op.ImportOp where

import Binja.Prelude


data ImportOp expr = ImportOp
    { _importOpConstant :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (ImportOp a)