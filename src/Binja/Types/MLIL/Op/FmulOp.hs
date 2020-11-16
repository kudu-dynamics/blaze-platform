module Binja.Types.MLIL.Op.FmulOp where

import Binja.Prelude


data FmulOp expr = FmulOp
    { _fmulOpLeft :: expr
    , _fmulOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (FmulOp a)
instance Serial m a => Serial m (FmulOp a)