module Binja.Types.MLIL.Op.SetVarSplitOp where

import Binja.Prelude

import Binja.Types.Variable (Variable)

data SetVarSplitOp expr = SetVarSplitOp
    { _setVarSplitOpHigh :: Variable
    , _setVarSplitOpLow :: Variable
    , _setVarSplitOpSrc :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
