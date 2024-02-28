module Binja.Types.MLIL.Op.VarSplitOp where

import Binja.Prelude

import Binja.Types.Variable (Variable)

data VarSplitOp expr = VarSplitOp
    { _varSplitOpHigh :: Variable
    , _varSplitOpLow :: Variable
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
