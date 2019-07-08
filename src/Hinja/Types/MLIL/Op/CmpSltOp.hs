module Hinja.Types.MLIL.Op.CmpSltOp where

import Hinja.Prelude


data CmpSltOp expr = CmpSltOp
    { _cmpSltOpLeft :: expr
    , _cmpSltOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
