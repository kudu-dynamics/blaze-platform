module Binja.Types.MLIL.Op.CmpSgtOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data CmpSgtOp expr = CmpSgtOp
    { _cmpSgtOpLeft :: expr
    , _cmpSgtOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (CmpSgtOp a)
instance Hashable a => Hashable (CmpSgtOp a)