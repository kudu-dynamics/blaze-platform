module Binja.Types.MLIL.Op.BpOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data BpOp expr = BpOp
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (BpOp a)
instance Hashable a => Hashable (BpOp a)