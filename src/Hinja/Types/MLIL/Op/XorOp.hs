module Hinja.Types.MLIL.Op.XorOp where

import Hinja.Prelude


data XorOp expr = XorOp
    { _xorOpLeft :: expr
    , _xorOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
