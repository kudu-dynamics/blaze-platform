module Hinja.Types.MLIL.Op.SetVarOp where

import Hinja.Prelude

import Hinja.Types.Variable (Variable)

data SetVarOp expr = SetVarOp
    { _setVarOpDest :: Variable
    , _setVarOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
