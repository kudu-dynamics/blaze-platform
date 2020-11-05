module Binja.Types.MLIL.Op.IntToFloatOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data IntToFloatOp expr = IntToFloatOp
    { _intToFloatOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (IntToFloatOp a)
instance Hashable a => Hashable (IntToFloatOp a)