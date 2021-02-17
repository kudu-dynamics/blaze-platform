module Binja.Types.MLIL.Op.CallOutputOp where

import Binja.Prelude

import Binja.Types.Variable (Variable)

newtype CallOutputOp expr = CallOutputOp
    { _callOutputOpDest :: [Variable]
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
