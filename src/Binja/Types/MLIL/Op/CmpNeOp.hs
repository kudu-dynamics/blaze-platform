module Binja.Types.MLIL.Op.CmpNeOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data CmpNeOp expr = CmpNeOp
    { _cmpNeOpLeft :: expr
    , _cmpNeOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (CmpNeOp a)
instance Hashable a => Hashable (CmpNeOp a)