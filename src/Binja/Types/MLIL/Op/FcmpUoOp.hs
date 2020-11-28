module Binja.Types.MLIL.Op.FcmpUoOp where

import Binja.Prelude


data FcmpUoOp expr = FcmpUoOp
    { _fcmpUoOpLeft :: expr
    , _fcmpUoOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (FcmpUoOp a)
instance Serial m a => Serial m (FcmpUoOp a)
