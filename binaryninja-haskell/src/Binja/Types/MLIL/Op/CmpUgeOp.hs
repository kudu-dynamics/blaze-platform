module Binja.Types.MLIL.Op.CmpUgeOp where

import Binja.Prelude


data CmpUgeOp expr = CmpUgeOp
    { _cmpUgeOpLeft :: expr
    , _cmpUgeOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
