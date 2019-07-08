module Hinja.Types.MLIL.Op.MuluDpOp where

import Hinja.Prelude


data MuluDpOp expr = MuluDpOp
    { _muluDpOpLeft :: expr
    , _muluDpOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
