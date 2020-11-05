module Binja.Types.MLIL.Op.CallParamOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)

import Binja.Types.Variable (Variable)

data CallParamOp expr = CallParamOp
    { _callParamOpSrc :: [Variable]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (CallParamOp a)
instance Hashable a => Hashable (CallParamOp a)