module Binja.Types.MLIL.Op.VarPhiOp where

import Binja.Prelude

import Binja.Types.MLIL.Common (SSAVariable)

data VarPhiOp expr = VarPhiOp
    { _varPhiOpDest :: SSAVariable
    , _varPhiOpSrc :: [SSAVariable]
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
