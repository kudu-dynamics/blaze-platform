module Binja.Types.MLIL.Op.VarOp where

import Binja.Prelude

import Binja.Types.Variable (Variable)

newtype VarOp expr = VarOp
    { _varOpSrc :: Variable
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
