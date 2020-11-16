module Binja.Types.MLIL.Op.VarSSAOp where

import Binja.Prelude

import Binja.Types.MLIL.Common (SSAVariable)

data VarSSAOp expr = VarSSAOp
    { _varSSAOpSrc :: SSAVariable
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (VarSSAOp a)
instance Hashable a => Hashable (VarSSAOp a)