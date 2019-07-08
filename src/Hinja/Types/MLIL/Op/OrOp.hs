module Hinja.Types.MLIL.Op.OrOp where

import Hinja.Prelude


data OrOp expr = OrOp
    { _orOpLeft :: expr
    , _orOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
