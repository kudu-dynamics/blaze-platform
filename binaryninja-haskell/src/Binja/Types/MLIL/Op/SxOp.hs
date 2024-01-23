module Binja.Types.MLIL.Op.SxOp where

import Binja.Prelude


data SxOp expr = SxOp
    { _sxOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (SxOp a)
