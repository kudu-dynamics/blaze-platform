module Binja.Types.MLIL.Op.TailcallOp where

import Binja.Prelude

import Binja.Types.Variable (Variable)

data TailcallOp expr = TailcallOp
    { _tailcallOpOutput :: [Variable]
    , _tailcallOpDest :: expr
    , _tailcallOpParams :: [expr]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (TailcallOp a)