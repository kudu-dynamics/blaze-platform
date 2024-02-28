module Binja.Types.MLIL.Op.IntrinsicSSAOp where

import Binja.Prelude

import Binja.Types.MLIL.Common (Intrinsic, SSAVariable)

data IntrinsicSSAOp expr = IntrinsicSSAOp
    { _intrinsicSSAOpOutput :: [SSAVariable]
    , _intrinsicSSAOpIntrinsic :: Intrinsic
    , _intrinsicSSAOpParams :: [expr]
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
