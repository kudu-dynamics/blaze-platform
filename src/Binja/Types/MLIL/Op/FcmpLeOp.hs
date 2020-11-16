module Binja.Types.MLIL.Op.FcmpLeOp where

import Binja.Prelude


data FcmpLeOp expr = FcmpLeOp
    { _fcmpLeOpLeft :: expr
    , _fcmpLeOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (FcmpLeOp a)
instance Hashable a => Hashable (FcmpLeOp a)