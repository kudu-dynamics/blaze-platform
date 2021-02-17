module Binja.Types.MLIL.Op.VarFieldOp where

import Binja.Prelude

import Binja.Types.Variable (Variable)

data VarFieldOp expr = VarFieldOp
    { _varFieldOpSrc :: Variable
    , _varFieldOpOffset :: Int64
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
