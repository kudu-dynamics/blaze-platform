module Binja.Types.MLIL.Op.RlcOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data RlcOp expr = RlcOp
    { _rlcOpLeft :: expr
    , _rlcOpRight :: expr
    , _rlcOpCarry :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (RlcOp a)
instance Hashable a => Hashable (RlcOp a)