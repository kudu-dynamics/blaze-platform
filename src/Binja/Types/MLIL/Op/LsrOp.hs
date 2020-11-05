module Binja.Types.MLIL.Op.LsrOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data LsrOp expr = LsrOp
    { _lsrOpLeft :: expr
    , _lsrOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (LsrOp a)
instance Hashable a => Hashable (LsrOp a)