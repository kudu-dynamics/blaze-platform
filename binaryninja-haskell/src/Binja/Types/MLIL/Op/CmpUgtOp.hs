module Binja.Types.MLIL.Op.CmpUgtOp where

import Binja.Prelude


data CmpUgtOp expr = CmpUgtOp
    { _cmpUgtOpLeft :: expr
    , _cmpUgtOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
