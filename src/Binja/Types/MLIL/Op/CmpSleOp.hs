module Binja.Types.MLIL.Op.CmpSleOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data CmpSleOp expr = CmpSleOp
    { _cmpSleOpLeft :: expr
    , _cmpSleOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (CmpSleOp a)
instance Hashable a => Hashable (CmpSleOp a)