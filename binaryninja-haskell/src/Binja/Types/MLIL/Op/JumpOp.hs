module Binja.Types.MLIL.Op.JumpOp where

import Binja.Prelude


newtype JumpOp expr = JumpOp
    { _jumpOpDest :: expr
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
