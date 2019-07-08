module Hinja.Types.MLIL.Op.FaddOp where

import Hinja.Prelude


data FaddOp expr = FaddOp
    { _faddOpLeft :: expr
    , _faddOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
