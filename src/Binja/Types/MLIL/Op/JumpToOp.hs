module Binja.Types.MLIL.Op.JumpToOp where

import Binja.Prelude


data JumpToOp expr = JumpToOp
    { _jumpToOpDest :: expr
    , _jumpToOpTargets :: [Int64]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (JumpToOp a)
instance Hashable a => Hashable (JumpToOp a)