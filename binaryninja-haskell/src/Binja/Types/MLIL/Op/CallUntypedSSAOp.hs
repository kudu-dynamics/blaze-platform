module Binja.Types.MLIL.Op.CallUntypedSSAOp where

import Binja.Prelude


data CallUntypedSSAOp expr = CallUntypedSSAOp
    { _callUntypedSSAOpOutput :: expr
    , _callUntypedSSAOpDest :: expr
    , _callUntypedSSAOpParams :: expr
    , _callUntypedSSAOpStack :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
