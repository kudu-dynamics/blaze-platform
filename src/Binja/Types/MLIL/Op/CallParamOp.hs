module Binja.Types.MLIL.Op.CallParamOp where

import Binja.Prelude

import Binja.Types.Variable (Variable)

data CallParamOp expr = CallParamOp
    { _callParamOpSrc :: [Variable]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (CallParamOp a)
instance Serial m a => Serial m (CallParamOp a)
