module Binja.Types.MLIL.Op.RrcOp where

import Binja.Prelude


data RrcOp expr = RrcOp
    { _rrcOpLeft :: expr
    , _rrcOpRight :: expr
    , _rrcOpCarry :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (RrcOp a)