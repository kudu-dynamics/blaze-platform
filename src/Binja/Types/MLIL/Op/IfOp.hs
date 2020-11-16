module Binja.Types.MLIL.Op.IfOp where

import Binja.Prelude


data IfOp expr = IfOp
    { _ifOpCondition :: expr
    , _ifOpTrue :: Int64
    , _ifOpFalse :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (IfOp a)
instance Hashable a => Hashable (IfOp a)