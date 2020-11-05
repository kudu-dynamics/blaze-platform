module Binja.Types.MLIL.Op.NopOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data NopOp expr = NopOp
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (NopOp a)
instance Hashable a => Hashable (NopOp a)