module Binja.Types.MLIL.Op.CallOutputSSAOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)

import Binja.Types.MLIL.Common (SSAVariable)

data CallOutputSSAOp expr = CallOutputSSAOp
    { _callOutputSSAOpDest_memory :: Int64
    , _callOutputSSAOpDest :: [SSAVariable]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (CallOutputSSAOp a)
instance Hashable a => Hashable (CallOutputSSAOp a)