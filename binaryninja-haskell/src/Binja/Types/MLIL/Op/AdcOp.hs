module Binja.Types.MLIL.Op.AdcOp where

import Binja.Prelude


data AdcOp expr = AdcOp
    { _adcOpLeft :: expr
    , _adcOpRight :: expr
    , _adcOpCarry :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
