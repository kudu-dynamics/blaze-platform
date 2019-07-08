module Hinja.Types.MLIL.Op.SetVarSplitSSAOp where

import Hinja.Prelude

import Hinja.Types.MLIL.Common (SSAVariable)

data SetVarSplitSSAOp expr = SetVarSplitSSAOp
    { _setVarSplitSSAOpHigh :: SSAVariable
    , _setVarSplitSSAOpLow :: SSAVariable
    , _setVarSplitSSAOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
