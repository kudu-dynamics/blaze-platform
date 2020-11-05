module Binja.Types.MLIL.Op.AdcOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data AdcOp expr = AdcOp
    { _adcOpLeft :: expr
    , _adcOpRight :: expr
    , _adcOpCarry :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (AdcOp a)
instance Hashable a => Hashable (AdcOp a)
