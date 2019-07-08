module Hinja.Types.MLIL.Op.MulOp where

import Hinja.Prelude


data MulOp expr = MulOp
    { _mulOpLeft :: expr
    , _mulOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
