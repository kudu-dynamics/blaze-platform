module Binja.Types.MLIL.Op.FcmpGeOp where

import Binja.Prelude


data FcmpGeOp expr = FcmpGeOp
    { _fcmpGeOpLeft :: expr
    , _fcmpGeOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
