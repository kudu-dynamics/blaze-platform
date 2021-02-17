module Binja.Types.MLIL.Op.CallOp where

import Binja.Prelude

import Binja.Types.Variable (Variable)

data CallOp expr = CallOp
    { _callOpOutput :: [Variable]
    , _callOpDest :: expr
    , _callOpParams :: [expr]
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
