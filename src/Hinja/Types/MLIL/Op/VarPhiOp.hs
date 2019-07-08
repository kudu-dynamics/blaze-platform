module Hinja.Types.MLIL.Op.VarPhiOp where

import Hinja.Prelude

import Hinja.Types.MLIL.Common (SSAVariable)
import Hinja.Types.MLIL.Common (SSAVariable)

data VarPhiOp expr = VarPhiOp
    { _varPhiOpDest :: SSAVariable
    , _varPhiOpSrc :: [SSAVariable]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
