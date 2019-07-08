module Hinja.Types.MLIL.Op.LsrOp where

import Hinja.Prelude


data LsrOp expr = LsrOp
    { _lsrOpLeft :: expr
    , _lsrOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
