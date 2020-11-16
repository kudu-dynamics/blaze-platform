module Binja.Types.MLIL.Op.FreeVarSlotOp where

import Binja.Prelude

import Binja.Types.Variable (Variable)

data FreeVarSlotOp expr = FreeVarSlotOp
    { _freeVarSlotOpDest :: Variable
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (FreeVarSlotOp a)
instance Hashable a => Hashable (FreeVarSlotOp a)