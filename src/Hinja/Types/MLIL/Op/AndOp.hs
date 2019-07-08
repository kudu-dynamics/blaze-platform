module Hinja.Types.MLIL.Op.AndOp where

import Hinja.Prelude


data AndOp expr = AndOp
    { _andOpLeft :: expr
    , _andOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
