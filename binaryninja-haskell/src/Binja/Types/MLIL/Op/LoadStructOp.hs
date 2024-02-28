module Binja.Types.MLIL.Op.LoadStructOp where

import Binja.Prelude


data LoadStructOp expr = LoadStructOp
    { _loadStructOpSrc :: expr
    , _loadStructOpOffset :: Int64
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
