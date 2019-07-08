module Hinja.Types.MLIL.Op.SetVarSSAOp where

import Hinja.Prelude

import Hinja.Types.MLIL.Common (SSAVariable)

data SetVarSSAOp expr = SetVarSSAOp
    { _setVarSSAOpDest :: SSAVariable
    , _setVarSSAOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
