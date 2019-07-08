module Hinja.Types.MLIL.Op.SetVarAliasedFieldOp where

import Hinja.Prelude

import Hinja.Types.MLIL.Common (SSAVariableDestAndSrc)

data SetVarAliasedFieldOp expr = SetVarAliasedFieldOp
    { _setVarAliasedFieldOpPrev :: SSAVariableDestAndSrc
    , _setVarAliasedFieldOpOffset :: Int64
    , _setVarAliasedFieldOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
