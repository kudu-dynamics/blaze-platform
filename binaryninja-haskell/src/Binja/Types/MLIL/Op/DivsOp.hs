module Binja.Types.MLIL.Op.DivsOp where

import Binja.Prelude


data DivsOp expr = DivsOp
    { _divsOpLeft :: expr
    , _divsOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
