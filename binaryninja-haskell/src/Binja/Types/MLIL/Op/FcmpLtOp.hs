module Binja.Types.MLIL.Op.FcmpLtOp where

import Binja.Prelude


data FcmpLtOp expr = FcmpLtOp
    { _fcmpLtOpLeft :: expr
    , _fcmpLtOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
