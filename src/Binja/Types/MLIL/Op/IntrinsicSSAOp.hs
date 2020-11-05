module Binja.Types.MLIL.Op.IntrinsicSSAOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)

import Binja.Types.MLIL.Common (Intrinsic)
import Binja.Types.MLIL.Common (SSAVariable)

data IntrinsicSSAOp expr = IntrinsicSSAOp
    { _intrinsicSSAOpOutput :: [SSAVariable]
    , _intrinsicSSAOpIntrinsic :: Intrinsic
    , _intrinsicSSAOpParams :: [expr]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (IntrinsicSSAOp a)
instance Hashable a => Hashable (IntrinsicSSAOp a)