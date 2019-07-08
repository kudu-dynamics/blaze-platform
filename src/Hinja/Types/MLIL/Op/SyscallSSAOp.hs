module Hinja.Types.MLIL.Op.SyscallSSAOp where

import Hinja.Prelude


data SyscallSSAOp expr = SyscallSSAOp
    { _syscallSSAOpOutput :: expr
    , _syscallSSAOpParams :: [expr]
    , _syscallSSAOpSrc_memory :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
