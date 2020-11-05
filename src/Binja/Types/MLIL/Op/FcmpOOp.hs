module Binja.Types.MLIL.Op.FcmpOOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data FcmpOOp expr = FcmpOOp
    { _fcmpOOpLeft :: expr
    , _fcmpOOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (FcmpOOp a)
instance Hashable a => Hashable (FcmpOOp a)