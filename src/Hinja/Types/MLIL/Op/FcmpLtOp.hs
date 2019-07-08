module Hinja.Types.MLIL.Op.FcmpLtOp where

import Hinja.Prelude


data FcmpLtOp expr = FcmpLtOp
    { _fcmpLtOpLeft :: expr
    , _fcmpLtOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
