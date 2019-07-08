module Hinja.Types.MLIL.Op.CmpUltOp where

import Hinja.Prelude


data CmpUltOp expr = CmpUltOp
    { _cmpUltOpLeft :: expr
    , _cmpUltOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
