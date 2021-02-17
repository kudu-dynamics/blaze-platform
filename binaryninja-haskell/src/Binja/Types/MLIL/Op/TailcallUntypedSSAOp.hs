module Binja.Types.MLIL.Op.TailcallUntypedSSAOp where

import Binja.Prelude


data TailcallUntypedSSAOp expr = TailcallUntypedSSAOp
    { _tailcallUntypedSSAOpOutput :: expr
    , _tailcallUntypedSSAOpDest :: expr
    , _tailcallUntypedSSAOpParams :: expr
    , _tailcallUntypedSSAOpStack :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
