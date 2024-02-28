module Binja.Types.MLIL.Op.FcmpUoOp where

import Binja.Prelude


data FcmpUoOp expr = FcmpUoOp
    { _fcmpUoOpLeft :: expr
    , _fcmpUoOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
