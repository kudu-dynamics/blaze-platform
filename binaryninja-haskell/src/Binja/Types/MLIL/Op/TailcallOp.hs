module Binja.Types.MLIL.Op.TailcallOp where

import Binja.Prelude

import Binja.Types.Variable (Variable)

data TailcallOp expr = TailcallOp
    { _tailcallOpOutput :: [Variable]
    , _tailcallOpDest :: expr
    , _tailcallOpParams :: [expr]
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
