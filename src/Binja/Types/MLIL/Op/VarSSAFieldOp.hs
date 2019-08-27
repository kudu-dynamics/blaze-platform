module Binja.Types.MLIL.Op.VarSSAFieldOp where

import Binja.Prelude

import Binja.Types.MLIL.Common (SSAVariable)

data VarSSAFieldOp expr = VarSSAFieldOp
    { _varSSAFieldOpSrc :: SSAVariable
    , _varSSAFieldOpOffset :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
