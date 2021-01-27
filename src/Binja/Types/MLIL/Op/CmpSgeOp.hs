module Binja.Types.MLIL.Op.CmpSgeOp where

import Binja.Prelude


data CmpSgeOp expr = CmpSgeOp
    { _cmpSgeOpLeft :: expr
    , _cmpSgeOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (CmpSgeOp a)
