module Hinja.Types.MLIL.Op.SubOp where

import Hinja.Prelude


data SubOp expr = SubOp
    { _subOpLeft :: expr
    , _subOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
