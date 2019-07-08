module Hinja.Types.MLIL.Op.FmulOp where

import Hinja.Prelude


data FmulOp expr = FmulOp
    { _fmulOpLeft :: expr
    , _fmulOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
