module Hinja.Types.MLIL.Op.RorOp where

import Hinja.Prelude


data RorOp expr = RorOp
    { _rorOpLeft :: expr
    , _rorOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
