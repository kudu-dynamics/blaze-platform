module Hinja.Types.MLIL.Op.AsrOp where

import Hinja.Prelude


data AsrOp expr = AsrOp
    { _asrOpLeft :: expr
    , _asrOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
