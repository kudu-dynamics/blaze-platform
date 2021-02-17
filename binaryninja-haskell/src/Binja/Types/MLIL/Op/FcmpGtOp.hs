module Binja.Types.MLIL.Op.FcmpGtOp where

import Binja.Prelude


data FcmpGtOp expr = FcmpGtOp
    { _fcmpGtOpLeft :: expr
    , _fcmpGtOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
