module Binja.Types.MLIL.Op.SetVarSplitSSAOp where

import Binja.Prelude

import Binja.Types.MLIL.Common (SSAVariable)

data SetVarSplitSSAOp expr = SetVarSplitSSAOp
    { _setVarSplitSSAOpHigh :: SSAVariable
    , _setVarSplitSSAOpLow :: SSAVariable
    , _setVarSplitSSAOpSrc :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
