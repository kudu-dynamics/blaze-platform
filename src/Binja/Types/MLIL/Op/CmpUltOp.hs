module Binja.Types.MLIL.Op.CmpUltOp where

import Binja.Prelude


data CmpUltOp expr = CmpUltOp
    { _cmpUltOpLeft :: expr
    , _cmpUltOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (CmpUltOp a)
