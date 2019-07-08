module Hinja.Types.MLIL.Op.FsubOp where

import Hinja.Prelude


data FsubOp expr = FsubOp
    { _fsubOpLeft :: expr
    , _fsubOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
