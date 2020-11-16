module Binja.Types.MLIL.Op.FcmpGeOp where

import Binja.Prelude


data FcmpGeOp expr = FcmpGeOp
    { _fcmpGeOpLeft :: expr
    , _fcmpGeOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (FcmpGeOp a)
instance Hashable a => Hashable (FcmpGeOp a)