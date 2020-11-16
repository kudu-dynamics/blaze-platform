module Binja.Types.MLIL.Op.SetVarSSAOp where

import Binja.Prelude

import Binja.Types.MLIL.Common (SSAVariable)

data SetVarSSAOp expr = SetVarSSAOp
    { _setVarSSAOpDest :: SSAVariable
    , _setVarSSAOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (SetVarSSAOp a)
instance Hashable a => Hashable (SetVarSSAOp a)