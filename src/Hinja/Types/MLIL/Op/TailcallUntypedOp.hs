module Hinja.Types.MLIL.Op.TailcallUntypedOp where

import Hinja.Prelude


data TailcallUntypedOp expr = TailcallUntypedOp
    { _tailcallUntypedOpOutput :: expr
    , _tailcallUntypedOpDest :: expr
    , _tailcallUntypedOpParams :: expr
    , _tailcallUntypedOpStack :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
