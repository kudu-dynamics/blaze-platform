module Binja.Types.MLIL.Op.CallOutputSSAOp where

import Binja.Prelude

import Binja.Types.MLIL.Common (SSAVariable)

data CallOutputSSAOp expr = CallOutputSSAOp
    { _callOutputSSAOpDest_memory :: Int64
    , _callOutputSSAOpDest :: [SSAVariable]
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
