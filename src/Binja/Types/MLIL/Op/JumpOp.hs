module Binja.Types.MLIL.Op.JumpOp where

import Binja.Prelude


data JumpOp expr = JumpOp
    { _jumpOpDest :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
