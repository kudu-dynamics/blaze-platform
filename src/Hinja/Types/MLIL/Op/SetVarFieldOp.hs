module Hinja.Types.MLIL.Op.SetVarFieldOp where

import Hinja.Prelude

import Hinja.Types.Variable (Variable)

data SetVarFieldOp expr = SetVarFieldOp
    { _setVarFieldOpDest :: Variable
    , _setVarFieldOpOffset :: Int64
    , _setVarFieldOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
