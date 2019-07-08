module Hinja.Types.MLIL.Op.FcmpGeOp where

import Hinja.Prelude


data FcmpGeOp expr = FcmpGeOp
    { _fcmpGeOpLeft :: expr
    , _fcmpGeOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
