module Hinja.Types.MLIL.Op.SyscallUntypedOp where

import Hinja.Prelude


data SyscallUntypedOp expr = SyscallUntypedOp
    { _syscallUntypedOpOutput :: expr
    , _syscallUntypedOpParams :: expr
    , _syscallUntypedOpStack :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
