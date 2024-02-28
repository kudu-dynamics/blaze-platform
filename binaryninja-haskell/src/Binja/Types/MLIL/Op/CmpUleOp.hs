module Binja.Types.MLIL.Op.CmpUleOp where

import Binja.Prelude


data CmpUleOp expr = CmpUleOp
    { _cmpUleOpLeft :: expr
    , _cmpUleOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
