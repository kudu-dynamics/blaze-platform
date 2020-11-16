module Binja.Types.MLIL.Op.AddOverflowOp where

import Binja.Prelude


data AddOverflowOp expr = AddOverflowOp
    { _addOverflowOpLeft :: expr
    , _addOverflowOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (AddOverflowOp a)
instance Hashable a => Hashable (AddOverflowOp a)