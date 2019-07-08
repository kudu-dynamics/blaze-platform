module Hinja.Types.MLIL.Op.FcmpGtOp where

import Hinja.Prelude


data FcmpGtOp expr = FcmpGtOp
    { _fcmpGtOpLeft :: expr
    , _fcmpGtOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
