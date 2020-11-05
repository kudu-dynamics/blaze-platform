module Binja.Types.MLIL.Op.IntrinsicOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)

import Binja.Types.MLIL.Common (Intrinsic)
import Binja.Types.Variable (Variable)

data IntrinsicOp expr = IntrinsicOp
    { _intrinsicOpOutput :: [Variable]
    , _intrinsicOpIntrinsic :: Intrinsic
    , _intrinsicOpParams :: [expr]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (IntrinsicOp a)
instance Hashable a => Hashable (IntrinsicOp a)