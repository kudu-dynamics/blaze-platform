module Binja.Types.MLIL.Op.CmpSleOp where

import Binja.Prelude


data CmpSleOp expr = CmpSleOp
    { _cmpSleOpLeft :: expr
    , _cmpSleOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (CmpSleOp a)
instance Serial m a => Serial m (CmpSleOp a)
