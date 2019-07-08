module Hinja.Types.MLIL.Op.RrcOp where

import Hinja.Prelude


data RrcOp expr = RrcOp
    { _rrcOpLeft :: expr
    , _rrcOpRight :: expr
    , _rrcOpCarry :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
