module Binja.Types.MLIL.Op.SubOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data SubOp expr = SubOp
    { _subOpLeft :: expr
    , _subOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (SubOp a)
instance Hashable a => Hashable (SubOp a)