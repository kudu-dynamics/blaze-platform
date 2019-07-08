module Hinja.Types.MLIL.Op.SyscallOp where

import Hinja.Prelude

import Hinja.Types.Variable (Variable)

data SyscallOp expr = SyscallOp
    { _syscallOpOutput :: [Variable]
    , _syscallOpParams :: [expr]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
