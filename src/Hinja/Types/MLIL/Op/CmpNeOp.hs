module Hinja.Types.MLIL.Op.CmpNeOp where

import Hinja.Prelude


data CmpNeOp expr = CmpNeOp
    { _cmpNeOpLeft :: expr
    , _cmpNeOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
