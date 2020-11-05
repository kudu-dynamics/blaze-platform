module Binja.Types.MLIL.Op.FcmpEOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data FcmpEOp expr = FcmpEOp
    { _fcmpEOpLeft :: expr
    , _fcmpEOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (FcmpEOp a)
instance Hashable a => Hashable (FcmpEOp a)