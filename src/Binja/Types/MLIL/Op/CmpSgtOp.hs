module Binja.Types.MLIL.Op.CmpSgtOp where

import Binja.Prelude


data CmpSgtOp expr = CmpSgtOp
    { _cmpSgtOpLeft :: expr
    , _cmpSgtOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
