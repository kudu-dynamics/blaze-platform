module Hinja.Types.MLIL.Op.CallParamOp where

import Hinja.Prelude

import Hinja.Types.Variable (Variable)

data CallParamOp expr = CallParamOp
    { _callParamOpSrc :: [Variable]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
