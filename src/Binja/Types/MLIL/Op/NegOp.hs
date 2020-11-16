module Binja.Types.MLIL.Op.NegOp where

import Binja.Prelude


data NegOp expr = NegOp
    { _negOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (NegOp a)
instance Hashable a => Hashable (NegOp a)