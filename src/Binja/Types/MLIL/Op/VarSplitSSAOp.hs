module Binja.Types.MLIL.Op.VarSplitSSAOp where

import Binja.Prelude

import Binja.Types.MLIL.Common (SSAVariable)

data VarSplitSSAOp expr = VarSplitSSAOp
    { _varSplitSSAOpHigh :: SSAVariable
    , _varSplitSSAOpLow :: SSAVariable
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (VarSplitSSAOp a)
instance Hashable a => Hashable (VarSplitSSAOp a)