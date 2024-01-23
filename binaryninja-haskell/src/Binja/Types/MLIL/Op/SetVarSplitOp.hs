module Binja.Types.MLIL.Op.SetVarSplitOp where

import Binja.Prelude

import Binja.Types.Variable (Variable)

data SetVarSplitOp expr = SetVarSplitOp
    { _setVarSplitOpHigh :: Variable
    , _setVarSplitOpLow :: Variable
    , _setVarSplitOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (SetVarSplitOp a)
