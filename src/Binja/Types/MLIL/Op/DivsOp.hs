module Binja.Types.MLIL.Op.DivsOp where

import Binja.Prelude


data DivsOp expr = DivsOp
    { _divsOpLeft :: expr
    , _divsOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (DivsOp a)