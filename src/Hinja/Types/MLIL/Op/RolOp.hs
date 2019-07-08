module Hinja.Types.MLIL.Op.RolOp where

import Hinja.Prelude


data RolOp expr = RolOp
    { _rolOpLeft :: expr
    , _rolOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
