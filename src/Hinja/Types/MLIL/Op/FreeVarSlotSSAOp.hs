module Hinja.Types.MLIL.Op.FreeVarSlotSSAOp where

import Hinja.Prelude

import Hinja.Types.MLIL.Common (SSAVariableDestAndSrc)

data FreeVarSlotSSAOp expr = FreeVarSlotSSAOp
    { _freeVarSlotSSAOpPrev :: SSAVariableDestAndSrc
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
