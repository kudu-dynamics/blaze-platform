module Binja.Types.MLIL.Op.ModsOp where

import Binja.Prelude


data ModsOp expr = ModsOp
    { _modsOpLeft :: expr
    , _modsOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
