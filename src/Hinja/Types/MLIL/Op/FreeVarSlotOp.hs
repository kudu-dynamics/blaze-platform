module Hinja.Types.MLIL.Op.FreeVarSlotOp where

import Hinja.Prelude

import Hinja.Types.Variable (Variable)

data FreeVarSlotOp expr = FreeVarSlotOp
    { _freeVarSlotOpDest :: Variable
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
