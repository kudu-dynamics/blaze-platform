module Binja.Types.MLIL.Op.SyscallUntypedSSAOp where

import Binja.Prelude


data SyscallUntypedSSAOp expr = SyscallUntypedSSAOp
    { _syscallUntypedSSAOpOutput :: expr
    , _syscallUntypedSSAOpParams :: expr
    , _syscallUntypedSSAOpStack :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (SyscallUntypedSSAOp a)
instance Hashable a => Hashable (SyscallUntypedSSAOp a)