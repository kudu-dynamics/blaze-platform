module Binja.Types.MLIL.Op.SyscallSSAOp where

import Binja.Prelude


data SyscallSSAOp expr = SyscallSSAOp
    { _syscallSSAOpOutput :: expr
    , _syscallSSAOpParams :: [expr]
    , _syscallSSAOpSrc_memory :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (SyscallSSAOp a)
instance Serial m a => Serial m (SyscallSSAOp a)