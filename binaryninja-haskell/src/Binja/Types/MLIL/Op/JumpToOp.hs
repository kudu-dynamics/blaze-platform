module Binja.Types.MLIL.Op.JumpToOp where

import Binja.Prelude


data JumpToOp expr = JumpToOp
    { _jumpToOpDest :: expr
    , _jumpToOpTargets :: [Address]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (JumpToOp a)
