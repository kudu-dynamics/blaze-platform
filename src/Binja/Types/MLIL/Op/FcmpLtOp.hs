module Binja.Types.MLIL.Op.FcmpLtOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data FcmpLtOp expr = FcmpLtOp
    { _fcmpLtOpLeft :: expr
    , _fcmpLtOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (FcmpLtOp a)
instance Hashable a => Hashable (FcmpLtOp a)