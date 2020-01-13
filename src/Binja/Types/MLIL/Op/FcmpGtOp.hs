module Binja.Types.MLIL.Op.FcmpGtOp where

import Binja.Prelude


data FcmpGtOp expr = FcmpGtOp
    { _fcmpGtOpLeft :: expr
    , _fcmpGtOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (FcmpGtOp a)