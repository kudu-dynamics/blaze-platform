module Binja.Types.MLIL.Op.SetVarOp where

import Binja.Prelude

import Binja.Types.Variable (Variable)

data SetVarOp expr = SetVarOp
    { _setVarOpDest :: Variable
    , _setVarOpSrc :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
