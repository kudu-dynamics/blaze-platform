module Binja.Types.MLIL.Op.RetHintOp where

import Binja.Prelude


newtype RetHintOp expr = RetHintOp
    { _retHintOpDest :: expr
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
