module Binja.Types.MLIL.Op.SetVarSSAFieldOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)

import Binja.Types.MLIL.Common (SSAVariableDestAndSrc)

data SetVarSSAFieldOp expr = SetVarSSAFieldOp
    { _setVarSSAFieldOpPrev :: SSAVariableDestAndSrc
    , _setVarSSAFieldOpOffset :: Int64
    , _setVarSSAFieldOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (SetVarSSAFieldOp a)
instance Hashable a => Hashable (SetVarSSAFieldOp a)