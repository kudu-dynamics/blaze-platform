module Binja.Types.MLIL.Op.CmpEOp where

import Binja.Prelude


data CmpEOp expr = CmpEOp
    { _cmpEOpLeft :: expr
    , _cmpEOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (CmpEOp a)
instance Hashable a => Hashable (CmpEOp a)