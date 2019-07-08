module Hinja.Types.MLIL.Op.IntrinsicSSAOp where

import Hinja.Prelude

import Hinja.Types.MLIL.Common (Intrinsic)
import Hinja.Types.MLIL.Common (SSAVariable)

data IntrinsicSSAOp expr = IntrinsicSSAOp
    { _intrinsicSSAOpOutput :: [SSAVariable]
    , _intrinsicSSAOpIntrinsic :: Intrinsic
    , _intrinsicSSAOpParams :: [expr]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
