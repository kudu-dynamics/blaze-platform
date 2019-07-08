module Hinja.Types.MLIL.Op.VarSplitSSAOp where

import Hinja.Prelude

import Hinja.Types.MLIL.Common (SSAVariable)

data VarSplitSSAOp expr = VarSplitSSAOp
    { _varSplitSSAOpHigh :: SSAVariable
    , _varSplitSSAOpLow :: SSAVariable
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
