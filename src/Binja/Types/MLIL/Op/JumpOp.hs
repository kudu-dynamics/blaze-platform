module Binja.Types.MLIL.Op.JumpOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data JumpOp expr = JumpOp
    { _jumpOpDest :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (JumpOp a)
instance Hashable a => Hashable (JumpOp a)