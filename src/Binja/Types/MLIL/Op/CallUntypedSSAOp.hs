module Binja.Types.MLIL.Op.CallUntypedSSAOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data CallUntypedSSAOp expr = CallUntypedSSAOp
    { _callUntypedSSAOpOutput :: expr
    , _callUntypedSSAOpDest :: expr
    , _callUntypedSSAOpParams :: expr
    , _callUntypedSSAOpStack :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (CallUntypedSSAOp a)
instance Hashable a => Hashable (CallUntypedSSAOp a)