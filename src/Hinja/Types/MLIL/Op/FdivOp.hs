module Hinja.Types.MLIL.Op.FdivOp where

import Hinja.Prelude


data FdivOp expr = FdivOp
    { _fdivOpLeft :: expr
    , _fdivOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
