module Hinja.Types.MLIL.Op.FcmpUoOp where

import Hinja.Prelude


data FcmpUoOp expr = FcmpUoOp
    { _fcmpUoOpLeft :: expr
    , _fcmpUoOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
