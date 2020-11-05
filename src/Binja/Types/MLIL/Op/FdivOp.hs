module Binja.Types.MLIL.Op.FdivOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data FdivOp expr = FdivOp
    { _fdivOpLeft :: expr
    , _fdivOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (FdivOp a)
instance Hashable a => Hashable (FdivOp a)