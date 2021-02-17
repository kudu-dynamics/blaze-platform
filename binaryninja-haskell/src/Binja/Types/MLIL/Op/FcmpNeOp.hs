module Binja.Types.MLIL.Op.FcmpNeOp where

import Binja.Prelude


data FcmpNeOp expr = FcmpNeOp
    { _fcmpNeOpLeft :: expr
    , _fcmpNeOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
