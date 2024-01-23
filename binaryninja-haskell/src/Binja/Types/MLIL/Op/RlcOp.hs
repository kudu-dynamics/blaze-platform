module Binja.Types.MLIL.Op.RlcOp where

import Binja.Prelude


data RlcOp expr = RlcOp
    { _rlcOpLeft :: expr
    , _rlcOpRight :: expr
    , _rlcOpCarry :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (RlcOp a)
