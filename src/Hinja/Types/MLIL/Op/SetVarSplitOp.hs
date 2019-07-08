module Hinja.Types.MLIL.Op.SetVarSplitOp where

import Hinja.Prelude

import Hinja.Types.Variable (Variable)

data SetVarSplitOp expr = SetVarSplitOp
    { _setVarSplitOpHigh :: Variable
    , _setVarSplitOpLow :: Variable
    , _setVarSplitOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
