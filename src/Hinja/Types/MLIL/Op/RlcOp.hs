module Hinja.Types.MLIL.Op.RlcOp where

import Hinja.Prelude


data RlcOp expr = RlcOp
    { _rlcOpLeft :: expr
    , _rlcOpRight :: expr
    , _rlcOpCarry :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
