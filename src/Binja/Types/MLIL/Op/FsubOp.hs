module Binja.Types.MLIL.Op.FsubOp where

import Binja.Prelude


data FsubOp expr = FsubOp
    { _fsubOpLeft :: expr
    , _fsubOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (FsubOp a)
instance Hashable a => Hashable (FsubOp a)