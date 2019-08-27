module Binja.Types.MLIL.Op.CallUntypedOp where

import Binja.Prelude


data CallUntypedOp expr = CallUntypedOp
    { _callUntypedOpOutput :: expr
    , _callUntypedOpDest :: expr
    , _callUntypedOpParams :: expr
    , _callUntypedOpStack :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
