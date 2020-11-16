module Binja.Types.MLIL.Op.ModsDpOp where

import Binja.Prelude


data ModsDpOp expr = ModsDpOp
    { _modsDpOpLeft :: expr
    , _modsDpOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (ModsDpOp a)
instance Hashable a => Hashable (ModsDpOp a)