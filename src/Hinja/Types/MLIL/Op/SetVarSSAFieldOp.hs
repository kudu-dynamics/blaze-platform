module Hinja.Types.MLIL.Op.SetVarSSAFieldOp where

import Hinja.Prelude

import Hinja.Types.MLIL.Common (SSAVariableDestAndSrc)

data SetVarSSAFieldOp expr = SetVarSSAFieldOp
    { _setVarSSAFieldOpPrev :: SSAVariableDestAndSrc
    , _setVarSSAFieldOpOffset :: Int64
    , _setVarSSAFieldOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
