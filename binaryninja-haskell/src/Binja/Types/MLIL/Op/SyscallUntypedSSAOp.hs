module Binja.Types.MLIL.Op.SyscallUntypedSSAOp where

import Binja.Prelude


data SyscallUntypedSSAOp expr = SyscallUntypedSSAOp
    { _syscallUntypedSSAOpOutput :: expr
    , _syscallUntypedSSAOpParams :: expr
    , _syscallUntypedSSAOpStack :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
