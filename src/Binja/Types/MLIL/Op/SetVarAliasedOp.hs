module Binja.Types.MLIL.Op.SetVarAliasedOp where

import Binja.Prelude

import Binja.Types.MLIL.Common (SSAVariableDestAndSrc)

data SetVarAliasedOp expr = SetVarAliasedOp
    { _setVarAliasedOpPrev :: SSAVariableDestAndSrc
    , _setVarAliasedOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (SetVarAliasedOp a)
instance Hashable a => Hashable (SetVarAliasedOp a)