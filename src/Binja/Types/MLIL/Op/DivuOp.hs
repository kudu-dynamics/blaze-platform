module Binja.Types.MLIL.Op.DivuOp where

import Binja.Prelude


data DivuOp expr = DivuOp
    { _divuOpLeft :: expr
    , _divuOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (DivuOp a)
instance Hashable a => Hashable (DivuOp a)