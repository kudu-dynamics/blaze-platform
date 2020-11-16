module Binja.Types.MLIL.Op.CallOutputSSAOp where

import Binja.Prelude

import Binja.Types.MLIL.Common (SSAVariable)

data CallOutputSSAOp expr = CallOutputSSAOp
    { _callOutputSSAOpDest_memory :: Int64
    , _callOutputSSAOpDest :: [SSAVariable]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (CallOutputSSAOp a)
instance Serial m a => Serial m (CallOutputSSAOp a)