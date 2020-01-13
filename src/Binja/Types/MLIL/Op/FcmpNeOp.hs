module Binja.Types.MLIL.Op.FcmpNeOp where

import Binja.Prelude


data FcmpNeOp expr = FcmpNeOp
    { _fcmpNeOpLeft :: expr
    , _fcmpNeOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (FcmpNeOp a)