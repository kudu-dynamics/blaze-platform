module Binja.Types.MLIL.Op.IntrinsicSSAOp where

import Binja.Prelude

import Binja.Types.MLIL.Common (Intrinsic)
import Binja.Types.MLIL.Common (SSAVariable)

data IntrinsicSSAOp expr = IntrinsicSSAOp
    { _intrinsicSSAOpOutput :: [SSAVariable]
    , _intrinsicSSAOpIntrinsic :: Intrinsic
    , _intrinsicSSAOpParams :: [expr]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (IntrinsicSSAOp a)