module Binja.Types.MLIL.Op.VarAliasedOp where

import Binja.Prelude

import Binja.Types.MLIL.Common (SSAVariable)

newtype VarAliasedOp expr = VarAliasedOp
    { _varAliasedOpSrc :: SSAVariable
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
