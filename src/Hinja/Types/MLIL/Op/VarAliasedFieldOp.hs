module Hinja.Types.MLIL.Op.VarAliasedFieldOp where

import Hinja.Prelude

import Hinja.Types.MLIL.Common (SSAVariable)

data VarAliasedFieldOp expr = VarAliasedFieldOp
    { _varAliasedFieldOpSrc :: SSAVariable
    , _varAliasedFieldOpOffset :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
