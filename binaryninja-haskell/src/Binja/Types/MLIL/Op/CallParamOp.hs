module Binja.Types.MLIL.Op.CallParamOp where

import Binja.Prelude

import Binja.Types.Variable (Variable)

newtype CallParamOp expr = CallParamOp
    { _callParamOpSrc :: [Variable]
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
