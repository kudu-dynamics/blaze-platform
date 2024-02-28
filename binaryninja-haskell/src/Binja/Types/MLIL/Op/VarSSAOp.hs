module Binja.Types.MLIL.Op.VarSSAOp where

import Binja.Prelude

import Binja.Types.MLIL.Common (SSAVariable)

newtype VarSSAOp expr = VarSSAOp
    { _varSSAOpSrc :: SSAVariable
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
