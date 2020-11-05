module Binja.Types.MLIL.Op.MulOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data MulOp expr = MulOp
    { _mulOpLeft :: expr
    , _mulOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (MulOp a)
instance Hashable a => Hashable (MulOp a)