module Binja.Types.MLIL.Op.SetVarFieldOp where

import Binja.Prelude

import Binja.Types.Variable (Variable)

data SetVarFieldOp expr = SetVarFieldOp
    { _setVarFieldOpDest :: Variable
    , _setVarFieldOpOffset :: Int64
    , _setVarFieldOpSrc :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
