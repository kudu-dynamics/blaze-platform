module Hinja.Types.MLIL.Op.SetVarAliasedOp where

import Hinja.Prelude

import Hinja.Types.MLIL.Common (SSAVariableDestAndSrc)

data SetVarAliasedOp expr = SetVarAliasedOp
    { _setVarAliasedOpPrev :: SSAVariableDestAndSrc
    , _setVarAliasedOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
