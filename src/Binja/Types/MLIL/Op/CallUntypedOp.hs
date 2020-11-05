module Binja.Types.MLIL.Op.CallUntypedOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data CallUntypedOp expr = CallUntypedOp
    { _callUntypedOpOutput :: expr
    , _callUntypedOpDest :: expr
    , _callUntypedOpParams :: expr
    , _callUntypedOpStack :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (CallUntypedOp a)
instance Hashable a => Hashable (CallUntypedOp a)