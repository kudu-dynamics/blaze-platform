module Hinja.Types.MLIL.Op.CallOutputSSAOp where

import Hinja.Prelude

import Hinja.Types.MLIL.Common (SSAVariable)

data CallOutputSSAOp expr = CallOutputSSAOp
    { _callOutputSSAOpDest_memory :: Int64
    , _callOutputSSAOpDest :: [SSAVariable]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
