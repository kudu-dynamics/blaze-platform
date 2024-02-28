module Binja.Types.MLIL.Op.CmpEOp where

import Binja.Prelude


data CmpEOp expr = CmpEOp
    { _cmpEOpLeft :: expr
    , _cmpEOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
