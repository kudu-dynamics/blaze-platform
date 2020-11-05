module Binja.Types.MLIL.Op.FreeVarSlotSSAOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)

import Binja.Types.MLIL.Common (SSAVariableDestAndSrc)

data FreeVarSlotSSAOp expr = FreeVarSlotSSAOp
    { _freeVarSlotSSAOpPrev :: SSAVariableDestAndSrc
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (FreeVarSlotSSAOp a)
instance Hashable a => Hashable (FreeVarSlotSSAOp a)