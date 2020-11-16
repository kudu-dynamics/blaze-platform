module Binja.Types.MLIL.Op.MulOp where

import Binja.Prelude


data MulOp expr = MulOp
    { _mulOpLeft :: expr
    , _mulOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (MulOp a)
instance Serial m a => Serial m (MulOp a)