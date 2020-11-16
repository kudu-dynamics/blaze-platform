module Binja.Types.MLIL.Op.RorOp where

import Binja.Prelude


data RorOp expr = RorOp
    { _rorOpLeft :: expr
    , _rorOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (RorOp a)
instance Hashable a => Hashable (RorOp a)