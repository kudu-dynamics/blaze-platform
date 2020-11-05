module Binja.Types.MLIL.Op.SyscallUntypedOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data SyscallUntypedOp expr = SyscallUntypedOp
    { _syscallUntypedOpOutput :: expr
    , _syscallUntypedOpParams :: expr
    , _syscallUntypedOpStack :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (SyscallUntypedOp a)
instance Hashable a => Hashable (SyscallUntypedOp a)