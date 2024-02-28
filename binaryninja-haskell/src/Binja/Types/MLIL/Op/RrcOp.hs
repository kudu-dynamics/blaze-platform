module Binja.Types.MLIL.Op.RrcOp where

import Binja.Prelude


data RrcOp expr = RrcOp
    { _rrcOpLeft :: expr
    , _rrcOpRight :: expr
    , _rrcOpCarry :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
