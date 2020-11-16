module Binja.Types.MLIL.Op.OrOp where

import Binja.Prelude


data OrOp expr = OrOp
    { _orOpLeft :: expr
    , _orOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (OrOp a)
instance Hashable a => Hashable (OrOp a)