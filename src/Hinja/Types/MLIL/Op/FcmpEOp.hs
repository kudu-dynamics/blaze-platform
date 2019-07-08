module Hinja.Types.MLIL.Op.FcmpEOp where

import Hinja.Prelude


data FcmpEOp expr = FcmpEOp
    { _fcmpEOpLeft :: expr
    , _fcmpEOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
