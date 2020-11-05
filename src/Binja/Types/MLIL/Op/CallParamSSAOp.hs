module Binja.Types.MLIL.Op.CallParamSSAOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)

import Binja.Types.MLIL.Common (SSAVariable)

data CallParamSSAOp expr = CallParamSSAOp
    { _callParamSSAOpSrc_memory :: Int64
    , _callParamSSAOpSrc :: [SSAVariable]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (CallParamSSAOp a)
instance Hashable a => Hashable (CallParamSSAOp a)