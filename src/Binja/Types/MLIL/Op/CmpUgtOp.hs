module Binja.Types.MLIL.Op.CmpUgtOp where

import Binja.Prelude


data CmpUgtOp expr = CmpUgtOp
    { _cmpUgtOpLeft :: expr
    , _cmpUgtOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (CmpUgtOp a)
instance Serial m a => Serial m (CmpUgtOp a)