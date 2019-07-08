module Hinja.Types.MLIL.Op.CmpUleOp where

import Hinja.Prelude


data CmpUleOp expr = CmpUleOp
    { _cmpUleOpLeft :: expr
    , _cmpUleOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
