module Binja.Types.MLIL.Op.TailcallUntypedOp where

import Binja.Prelude


data TailcallUntypedOp expr = TailcallUntypedOp
    { _tailcallUntypedOpOutput :: expr
    , _tailcallUntypedOpDest :: expr
    , _tailcallUntypedOpParams :: expr
    , _tailcallUntypedOpStack :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
