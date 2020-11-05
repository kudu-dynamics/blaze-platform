module Binja.Types.MLIL.Op.MuluDpOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data MuluDpOp expr = MuluDpOp
    { _muluDpOpLeft :: expr
    , _muluDpOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (MuluDpOp a)
instance Hashable a => Hashable (MuluDpOp a)