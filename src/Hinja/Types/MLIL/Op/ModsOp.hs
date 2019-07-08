module Hinja.Types.MLIL.Op.ModsOp where

import Hinja.Prelude


data ModsOp expr = ModsOp
    { _modsOpLeft :: expr
    , _modsOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
