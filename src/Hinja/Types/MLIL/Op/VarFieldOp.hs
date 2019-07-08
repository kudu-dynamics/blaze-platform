module Hinja.Types.MLIL.Op.VarFieldOp where

import Hinja.Prelude

import Hinja.Types.Variable (Variable)

data VarFieldOp expr = VarFieldOp
    { _varFieldOpSrc :: Variable
    , _varFieldOpOffset :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
