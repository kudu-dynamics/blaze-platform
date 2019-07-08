module Hinja.Types.MLIL.Op.VarSSAOp where

import Hinja.Prelude

import Hinja.Types.MLIL.Common (SSAVariable)

data VarSSAOp expr = VarSSAOp
    { _varSSAOpSrc :: SSAVariable
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
