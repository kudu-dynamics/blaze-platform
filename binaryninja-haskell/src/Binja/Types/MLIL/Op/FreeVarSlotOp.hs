module Binja.Types.MLIL.Op.FreeVarSlotOp where

import Binja.Prelude

import Binja.Types.Variable (Variable)

newtype FreeVarSlotOp expr = FreeVarSlotOp
    { _freeVarSlotOpDest :: Variable
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
