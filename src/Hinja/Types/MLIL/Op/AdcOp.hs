module Hinja.Types.MLIL.Op.AdcOp where

import Hinja.Prelude


data AdcOp expr = AdcOp
    { _adcOpLeft :: expr
    , _adcOpRight :: expr
    , _adcOpCarry :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
