module Binja.Types.MLIL.Op.SyscallOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)

import Binja.Types.Variable (Variable)

data SyscallOp expr = SyscallOp
    { _syscallOpOutput :: [Variable]
    , _syscallOpParams :: [expr]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (SyscallOp a)
instance Hashable a => Hashable (SyscallOp a)