module Binja.Types.MLIL.Op.SetVarAliasedFieldOp where

import Binja.Prelude

import Binja.Types.MLIL.Common (SSAVariableDestAndSrc)

data SetVarAliasedFieldOp expr = SetVarAliasedFieldOp
    { _setVarAliasedFieldOpPrev :: SSAVariableDestAndSrc
    , _setVarAliasedFieldOpOffset :: Int64
    , _setVarAliasedFieldOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (SetVarAliasedFieldOp a)
