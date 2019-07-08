module Hinja.Types.MLIL.Op.JumpOp where

import Hinja.Prelude


data JumpOp expr = JumpOp
    { _jumpOpDest :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
