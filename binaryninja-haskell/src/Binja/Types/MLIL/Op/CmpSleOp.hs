module Binja.Types.MLIL.Op.CmpSleOp where

import Binja.Prelude


data CmpSleOp expr = CmpSleOp
    { _cmpSleOpLeft :: expr
    , _cmpSleOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
