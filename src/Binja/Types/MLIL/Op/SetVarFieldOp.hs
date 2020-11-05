module Binja.Types.MLIL.Op.SetVarFieldOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)

import Binja.Types.Variable (Variable)

data SetVarFieldOp expr = SetVarFieldOp
    { _setVarFieldOpDest :: Variable
    , _setVarFieldOpOffset :: Int64
    , _setVarFieldOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (SetVarFieldOp a)
instance Hashable a => Hashable (SetVarFieldOp a)