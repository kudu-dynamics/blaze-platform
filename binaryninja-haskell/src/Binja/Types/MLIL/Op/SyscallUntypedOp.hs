module Binja.Types.MLIL.Op.SyscallUntypedOp where

import Binja.Prelude


data SyscallUntypedOp expr = SyscallUntypedOp
    { _syscallUntypedOpOutput :: expr
    , _syscallUntypedOpParams :: expr
    , _syscallUntypedOpStack :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
