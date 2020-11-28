module Binja.Types.MLIL.Op.SubOp where

import Binja.Prelude


data SubOp expr = SubOp
    { _subOpLeft :: expr
    , _subOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (SubOp a)
instance Serial m a => Serial m (SubOp a)
