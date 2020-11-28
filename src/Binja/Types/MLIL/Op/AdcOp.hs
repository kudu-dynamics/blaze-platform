module Binja.Types.MLIL.Op.AdcOp where

import Binja.Prelude


data AdcOp expr = AdcOp
    { _adcOpLeft :: expr
    , _adcOpRight :: expr
    , _adcOpCarry :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (AdcOp a)
instance Serial m a => Serial m (AdcOp a)
