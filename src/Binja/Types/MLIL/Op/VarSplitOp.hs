module Binja.Types.MLIL.Op.VarSplitOp where

import Binja.Prelude

import Binja.Types.Variable (Variable)

data VarSplitOp expr = VarSplitOp
    { _varSplitOpHigh :: Variable
    , _varSplitOpLow :: Variable
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (VarSplitOp a)
instance Serial m a => Serial m (VarSplitOp a)