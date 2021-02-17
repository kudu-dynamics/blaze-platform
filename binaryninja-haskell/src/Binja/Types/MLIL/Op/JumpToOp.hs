module Binja.Types.MLIL.Op.JumpToOp where

import Binja.Prelude


data JumpToOp expr = JumpToOp
    { _jumpToOpDest :: expr
    , _jumpToOpTargets :: [Address]
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
