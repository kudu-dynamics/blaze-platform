module Hinja.Types.MLIL.Op.CmpSgeOp where

import Hinja.Prelude


data CmpSgeOp expr = CmpSgeOp
    { _cmpSgeOpLeft :: expr
    , _cmpSgeOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
