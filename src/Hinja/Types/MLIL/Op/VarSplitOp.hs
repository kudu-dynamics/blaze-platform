module Hinja.Types.MLIL.Op.VarSplitOp where

import Hinja.Prelude

import Hinja.Types.Variable (Variable)

data VarSplitOp expr = VarSplitOp
    { _varSplitOpHigh :: Variable
    , _varSplitOpLow :: Variable
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
