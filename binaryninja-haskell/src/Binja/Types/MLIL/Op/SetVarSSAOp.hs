module Binja.Types.MLIL.Op.SetVarSSAOp where

import Binja.Prelude

import Binja.Types.MLIL.Common (SSAVariable)

data SetVarSSAOp expr = SetVarSSAOp
    { _setVarSSAOpDest :: SSAVariable
    , _setVarSSAOpSrc :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
