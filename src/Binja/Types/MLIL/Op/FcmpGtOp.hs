module Binja.Types.MLIL.Op.FcmpGtOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data FcmpGtOp expr = FcmpGtOp
    { _fcmpGtOpLeft :: expr
    , _fcmpGtOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (FcmpGtOp a)
instance Hashable a => Hashable (FcmpGtOp a)