module Binja.Types.MLIL.Op.FsubOp where

import Binja.Prelude


data FsubOp expr = FsubOp
    { _fsubOpLeft :: expr
    , _fsubOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
