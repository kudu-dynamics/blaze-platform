module Binja.Types.MLIL.Op.SbbOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data SbbOp expr = SbbOp
    { _sbbOpLeft :: expr
    , _sbbOpRight :: expr
    , _sbbOpCarry :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (SbbOp a)
instance Hashable a => Hashable (SbbOp a)