module Hinja.Types.MLIL.Op.TailcallOp where

import Hinja.Prelude

import Hinja.Types.Variable (Variable)

data TailcallOp expr = TailcallOp
    { _tailcallOpOutput :: [Variable]
    , _tailcallOpDest :: expr
    , _tailcallOpParams :: [expr]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
