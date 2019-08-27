module Binja.Types.MLIL.Op.ModuDpOp where

import Binja.Prelude


data ModuDpOp expr = ModuDpOp
    { _moduDpOpLeft :: expr
    , _moduDpOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
