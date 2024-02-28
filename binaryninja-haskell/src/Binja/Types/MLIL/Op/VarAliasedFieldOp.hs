module Binja.Types.MLIL.Op.VarAliasedFieldOp where

import Binja.Prelude

import Binja.Types.MLIL.Common (SSAVariable)

data VarAliasedFieldOp expr = VarAliasedFieldOp
    { _varAliasedFieldOpSrc :: SSAVariable
    , _varAliasedFieldOpOffset :: Int64
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
