module Binja.Types.MLIL.Op.CmpSgeOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data CmpSgeOp expr = CmpSgeOp
    { _cmpSgeOpLeft :: expr
    , _cmpSgeOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (CmpSgeOp a)
instance Hashable a => Hashable (CmpSgeOp a)