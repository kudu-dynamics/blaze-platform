module Binja.Types.MLIL.Op.UnimplOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data UnimplOp expr = UnimplOp
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (UnimplOp a)
instance Hashable a => Hashable (UnimplOp a)