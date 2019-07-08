module Hinja.Types.MLIL.Op.CmpUgeOp where

import Hinja.Prelude


data CmpUgeOp expr = CmpUgeOp
    { _cmpUgeOpLeft :: expr
    , _cmpUgeOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
