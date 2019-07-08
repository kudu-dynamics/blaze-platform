module Hinja.Types.MLIL.Op.LoadStructOp where

import Hinja.Prelude


data LoadStructOp expr = LoadStructOp
    { _loadStructOpSrc :: expr
    , _loadStructOpOffset :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
