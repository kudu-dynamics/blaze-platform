module Binja.Types.MLIL.Op.VarSplitOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)

import Binja.Types.Variable (Variable)

data VarSplitOp expr = VarSplitOp
    { _varSplitOpHigh :: Variable
    , _varSplitOpLow :: Variable
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (VarSplitOp a)
instance Hashable a => Hashable (VarSplitOp a)