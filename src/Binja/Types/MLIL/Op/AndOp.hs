module Binja.Types.MLIL.Op.AndOp where

import Binja.Prelude


data AndOp expr = AndOp
    { _andOpLeft :: expr
    , _andOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (AndOp a)
instance Hashable a => Hashable (AndOp a)