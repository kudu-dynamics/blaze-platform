module Hinja.Types.MLIL.Op.CallOutputOp where

import Hinja.Prelude

import Hinja.Types.Variable (Variable)

data CallOutputOp expr = CallOutputOp
    { _callOutputOpDest :: [Variable]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
