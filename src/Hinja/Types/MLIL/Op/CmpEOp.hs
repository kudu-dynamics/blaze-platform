module Hinja.Types.MLIL.Op.CmpEOp where

import Hinja.Prelude


data CmpEOp expr = CmpEOp
    { _cmpEOpLeft :: expr
    , _cmpEOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
