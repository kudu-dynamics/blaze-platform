module Binja.Types.MLIL.Op.FdivOp where

import Binja.Prelude


data FdivOp expr = FdivOp
    { _fdivOpLeft :: expr
    , _fdivOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (FdivOp a)
