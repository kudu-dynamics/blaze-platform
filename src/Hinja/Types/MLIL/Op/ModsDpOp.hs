module Hinja.Types.MLIL.Op.ModsDpOp where

import Hinja.Prelude


data ModsDpOp expr = ModsDpOp
    { _modsDpOpLeft :: expr
    , _modsDpOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
