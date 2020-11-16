module Binja.Types.MLIL.Op.BoolToIntOp where

import Binja.Prelude


data BoolToIntOp expr = BoolToIntOp
    { _boolToIntOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (BoolToIntOp a)
instance Hashable a => Hashable (BoolToIntOp a)