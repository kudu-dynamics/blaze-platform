module Binja.Types.MLIL.Op.CmpSgeOp where

import Binja.Prelude


data CmpSgeOp expr = CmpSgeOp
    { _cmpSgeOpLeft :: expr
    , _cmpSgeOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
