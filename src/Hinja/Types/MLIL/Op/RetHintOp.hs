module Hinja.Types.MLIL.Op.RetHintOp where

import Hinja.Prelude


data RetHintOp expr = RetHintOp
    { _retHintOpDest :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
