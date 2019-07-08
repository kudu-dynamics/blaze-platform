module Hinja.Types.MLIL.Op.CmpUgtOp where

import Hinja.Prelude


data CmpUgtOp expr = CmpUgtOp
    { _cmpUgtOpLeft :: expr
    , _cmpUgtOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
