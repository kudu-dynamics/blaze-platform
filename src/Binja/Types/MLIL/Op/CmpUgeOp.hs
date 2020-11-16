module Binja.Types.MLIL.Op.CmpUgeOp where

import Binja.Prelude


data CmpUgeOp expr = CmpUgeOp
    { _cmpUgeOpLeft :: expr
    , _cmpUgeOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (CmpUgeOp a)
instance Hashable a => Hashable (CmpUgeOp a)