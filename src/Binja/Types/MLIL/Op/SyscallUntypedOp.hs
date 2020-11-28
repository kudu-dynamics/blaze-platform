module Binja.Types.MLIL.Op.SyscallUntypedOp where

import Binja.Prelude


data SyscallUntypedOp expr = SyscallUntypedOp
    { _syscallUntypedOpOutput :: expr
    , _syscallUntypedOpParams :: expr
    , _syscallUntypedOpStack :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (SyscallUntypedOp a)
instance Serial m a => Serial m (SyscallUntypedOp a)
