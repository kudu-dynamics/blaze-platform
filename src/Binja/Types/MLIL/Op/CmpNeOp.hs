module Binja.Types.MLIL.Op.CmpNeOp where

import Binja.Prelude


data CmpNeOp expr = CmpNeOp
    { _cmpNeOpLeft :: expr
    , _cmpNeOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (CmpNeOp a)
instance Serial m a => Serial m (CmpNeOp a)