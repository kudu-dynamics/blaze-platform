module Hinja.Types.MLIL.Op.JumpToOp where

import Hinja.Prelude


data JumpToOp expr = JumpToOp
    { _jumpToOpDest :: expr
    , _jumpToOpTargets :: [Int64]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
