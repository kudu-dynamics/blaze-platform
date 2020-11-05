module Binja.Types.MLIL.Op.CmpUgtOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data CmpUgtOp expr = CmpUgtOp
    { _cmpUgtOpLeft :: expr
    , _cmpUgtOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (CmpUgtOp a)
instance Hashable a => Hashable (CmpUgtOp a)