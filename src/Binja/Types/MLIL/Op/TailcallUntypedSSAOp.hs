module Binja.Types.MLIL.Op.TailcallUntypedSSAOp where

import Binja.Prelude


data TailcallUntypedSSAOp expr = TailcallUntypedSSAOp
    { _tailcallUntypedSSAOpOutput :: expr
    , _tailcallUntypedSSAOpDest :: expr
    , _tailcallUntypedSSAOpParams :: expr
    , _tailcallUntypedSSAOpStack :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (TailcallUntypedSSAOp a)
instance Hashable a => Hashable (TailcallUntypedSSAOp a)