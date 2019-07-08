module Hinja.Types.MLIL.Op.ModuOp where

import Hinja.Prelude


data ModuOp expr = ModuOp
    { _moduOpLeft :: expr
    , _moduOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
