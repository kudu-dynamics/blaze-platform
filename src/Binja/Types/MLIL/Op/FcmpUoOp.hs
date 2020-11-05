module Binja.Types.MLIL.Op.FcmpUoOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data FcmpUoOp expr = FcmpUoOp
    { _fcmpUoOpLeft :: expr
    , _fcmpUoOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (FcmpUoOp a)
instance Hashable a => Hashable (FcmpUoOp a)