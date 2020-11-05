module Binja.Types.MLIL.Op.DivsDpOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data DivsDpOp expr = DivsDpOp
    { _divsDpOpLeft :: expr
    , _divsDpOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (DivsDpOp a)
instance Hashable a => Hashable (DivsDpOp a)