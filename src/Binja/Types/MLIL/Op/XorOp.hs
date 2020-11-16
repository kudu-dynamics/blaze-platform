module Binja.Types.MLIL.Op.XorOp where

import Binja.Prelude


data XorOp expr = XorOp
    { _xorOpLeft :: expr
    , _xorOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (XorOp a)
instance Hashable a => Hashable (XorOp a)