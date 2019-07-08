module Hinja.Types.MLIL.Op.VarOp where

import Hinja.Prelude

import Hinja.Types.Variable (Variable)

data VarOp expr = VarOp
    { _varOpSrc :: Variable
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
