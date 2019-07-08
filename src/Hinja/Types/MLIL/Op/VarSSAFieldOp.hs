module Hinja.Types.MLIL.Op.VarSSAFieldOp where

import Hinja.Prelude

import Hinja.Types.MLIL.Common (SSAVariable)

data VarSSAFieldOp expr = VarSSAFieldOp
    { _varSSAFieldOpSrc :: SSAVariable
    , _varSSAFieldOpOffset :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
