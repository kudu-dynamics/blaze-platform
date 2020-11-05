module Binja.Types.MLIL.Op.AddOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data AddOp expr = AddOp
    { _addOpLeft :: expr
    , _addOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (AddOp a)
instance Hashable a => Hashable (AddOp a)