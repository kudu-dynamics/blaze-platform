module Hinja.Types.MLIL.Op.CallOp where

import Hinja.Prelude

import Hinja.Types.Variable (Variable)

data CallOp expr = CallOp
    { _callOpOutput :: [Variable]
    , _callOpDest :: expr
    , _callOpParams :: [expr]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
