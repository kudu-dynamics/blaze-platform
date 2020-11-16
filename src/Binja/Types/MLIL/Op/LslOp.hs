module Binja.Types.MLIL.Op.LslOp where

import Binja.Prelude


data LslOp expr = LslOp
    { _lslOpLeft :: expr
    , _lslOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (LslOp a)
instance Hashable a => Hashable (LslOp a)