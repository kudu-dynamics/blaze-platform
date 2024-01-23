module Binja.Types.MLIL.Op.FcmpLtOp where

import Binja.Prelude


data FcmpLtOp expr = FcmpLtOp
    { _fcmpLtOpLeft :: expr
    , _fcmpLtOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (FcmpLtOp a)
