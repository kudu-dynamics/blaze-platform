module Hinja.Types.MLIL.Op.ImportOp where

import Hinja.Prelude


data ImportOp expr = ImportOp
    { _importOpConstant :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
