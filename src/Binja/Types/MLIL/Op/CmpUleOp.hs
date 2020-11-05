module Binja.Types.MLIL.Op.CmpUleOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data CmpUleOp expr = CmpUleOp
    { _cmpUleOpLeft :: expr
    , _cmpUleOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (CmpUleOp a)
instance Hashable a => Hashable (CmpUleOp a)