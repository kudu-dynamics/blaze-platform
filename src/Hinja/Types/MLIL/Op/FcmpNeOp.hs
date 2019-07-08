module Hinja.Types.MLIL.Op.FcmpNeOp where

import Hinja.Prelude


data FcmpNeOp expr = FcmpNeOp
    { _fcmpNeOpLeft :: expr
    , _fcmpNeOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
