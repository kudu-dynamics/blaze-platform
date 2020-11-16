module Binja.Types.MLIL.Op.SetVarSplitSSAOp where

import Binja.Prelude

import Binja.Types.MLIL.Common (SSAVariable)

data SetVarSplitSSAOp expr = SetVarSplitSSAOp
    { _setVarSplitSSAOpHigh :: SSAVariable
    , _setVarSplitSSAOpLow :: SSAVariable
    , _setVarSplitSSAOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (SetVarSplitSSAOp a)
instance Serial m a => Serial m (SetVarSplitSSAOp a)