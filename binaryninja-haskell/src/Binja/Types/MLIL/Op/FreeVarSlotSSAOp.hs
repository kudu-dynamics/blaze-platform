module Binja.Types.MLIL.Op.FreeVarSlotSSAOp where

import Binja.Prelude

import Binja.Types.MLIL.Common (SSAVariableDestAndSrc)

newtype FreeVarSlotSSAOp expr = FreeVarSlotSSAOp
    { _freeVarSlotSSAOpPrev :: SSAVariableDestAndSrc
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
