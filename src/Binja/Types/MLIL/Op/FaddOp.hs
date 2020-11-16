module Binja.Types.MLIL.Op.FaddOp where

import Binja.Prelude


data FaddOp expr = FaddOp
    { _faddOpLeft :: expr
    , _faddOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (FaddOp a)
instance Serial m a => Serial m (FaddOp a)