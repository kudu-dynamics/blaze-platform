module Hinja.Types.MLIL.Op.VarAliasedOp where

import Hinja.Prelude

import Hinja.Types.MLIL.Common (SSAVariable)

data VarAliasedOp expr = VarAliasedOp
    { _varAliasedOpSrc :: SSAVariable
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
