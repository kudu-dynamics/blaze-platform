module Hinja.Types.MLIL.Op.FcmpOOp where

import Hinja.Prelude


data FcmpOOp expr = FcmpOOp
    { _fcmpOOpLeft :: expr
    , _fcmpOOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
