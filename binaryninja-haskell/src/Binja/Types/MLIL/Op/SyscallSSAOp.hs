module Binja.Types.MLIL.Op.SyscallSSAOp where

import Binja.Prelude


data SyscallSSAOp expr = SyscallSSAOp
    { _syscallSSAOpOutput :: expr
    , _syscallSSAOpParams :: [expr]
    , _syscallSSAOpSrc_memory :: Int64
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
