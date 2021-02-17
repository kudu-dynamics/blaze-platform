module Binja.Types.MLIL.Op.MuluDpOp where

import Binja.Prelude


data MuluDpOp expr = MuluDpOp
    { _muluDpOpLeft :: expr
    , _muluDpOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
