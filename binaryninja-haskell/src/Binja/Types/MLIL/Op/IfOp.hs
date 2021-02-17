module Binja.Types.MLIL.Op.IfOp where

import Binja.Prelude


data IfOp expr = IfOp
    { _ifOpCondition :: expr
    , _ifOpTrue :: Int64
    , _ifOpFalse :: Int64
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
