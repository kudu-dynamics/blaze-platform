module Binja.Types.MLIL.Op.FcmpEOp where

import Binja.Prelude


data FcmpEOp expr = FcmpEOp
    { _fcmpEOpLeft :: expr
    , _fcmpEOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
