module Binja.Types.MLIL.Op.CmpSltOp where

import Binja.Prelude


data CmpSltOp expr = CmpSltOp
    { _cmpSltOpLeft :: expr
    , _cmpSltOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
