module Binja.Types.MLIL.Op.SyscallOp where

import Binja.Prelude

import Binja.Types.Variable (Variable)

data SyscallOp expr = SyscallOp
    { _syscallOpOutput :: [Variable]
    , _syscallOpParams :: [expr]
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
