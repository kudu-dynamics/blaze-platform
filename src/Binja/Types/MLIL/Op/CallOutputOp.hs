module Binja.Types.MLIL.Op.CallOutputOp where

import Binja.Prelude

import Binja.Types.Variable (Variable)

data CallOutputOp expr = CallOutputOp
    { _callOutputOpDest :: [Variable]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
