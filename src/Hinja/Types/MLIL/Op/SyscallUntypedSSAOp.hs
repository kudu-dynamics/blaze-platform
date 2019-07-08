module Hinja.Types.MLIL.Op.SyscallUntypedSSAOp where

import Hinja.Prelude


data SyscallUntypedSSAOp expr = SyscallUntypedSSAOp
    { _syscallUntypedSSAOpOutput :: expr
    , _syscallUntypedSSAOpParams :: expr
    , _syscallUntypedSSAOpStack :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
