module Binja.Types.MLIL.Op.VarSplitSSAOp where

import Binja.Prelude

import Binja.Types.MLIL.Common (SSAVariable)

data VarSplitSSAOp expr = VarSplitSSAOp
    { _varSplitSSAOpHigh :: SSAVariable
    , _varSplitSSAOpLow :: SSAVariable
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
