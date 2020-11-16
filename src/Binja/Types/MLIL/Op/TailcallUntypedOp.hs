module Binja.Types.MLIL.Op.TailcallUntypedOp where

import Binja.Prelude


data TailcallUntypedOp expr = TailcallUntypedOp
    { _tailcallUntypedOpOutput :: expr
    , _tailcallUntypedOpDest :: expr
    , _tailcallUntypedOpParams :: expr
    , _tailcallUntypedOpStack :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (TailcallUntypedOp a)
instance Hashable a => Hashable (TailcallUntypedOp a)