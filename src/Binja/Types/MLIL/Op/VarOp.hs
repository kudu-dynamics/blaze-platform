module Binja.Types.MLIL.Op.VarOp where

import Binja.Prelude

import Binja.Types.Variable (Variable)

data VarOp expr = VarOp
    { _varOpSrc :: Variable
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
