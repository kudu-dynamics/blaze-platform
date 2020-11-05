module Binja.Types.MLIL.Op.CmpUltOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data CmpUltOp expr = CmpUltOp
    { _cmpUltOpLeft :: expr
    , _cmpUltOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (CmpUltOp a)
instance Hashable a => Hashable (CmpUltOp a)