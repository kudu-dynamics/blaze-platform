module Binja.Types.MLIL.Op.RetOp where

import Binja.Prelude


data RetOp expr = RetOp
    { _retOpSrc :: [expr]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (RetOp a)
