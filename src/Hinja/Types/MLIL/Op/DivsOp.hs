module Hinja.Types.MLIL.Op.DivsOp where

import Hinja.Prelude


data DivsOp expr = DivsOp
    { _divsOpLeft :: expr
    , _divsOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
