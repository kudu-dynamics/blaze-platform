module Binja.Types.MLIL.Op.CmpUleOp where

import Binja.Prelude


data CmpUleOp expr = CmpUleOp
    { _cmpUleOpLeft :: expr
    , _cmpUleOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (CmpUleOp a)
