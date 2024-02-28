module Binja.Types.MLIL.Op.CmpNeOp where

import Binja.Prelude


data CmpNeOp expr = CmpNeOp
    { _cmpNeOpLeft :: expr
    , _cmpNeOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
