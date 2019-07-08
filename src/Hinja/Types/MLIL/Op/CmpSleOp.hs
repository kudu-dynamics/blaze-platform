module Hinja.Types.MLIL.Op.CmpSleOp where

import Hinja.Prelude


data CmpSleOp expr = CmpSleOp
    { _cmpSleOpLeft :: expr
    , _cmpSleOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
