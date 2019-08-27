module Binja.Types.MLIL.Op.FcmpLeOp where

import Binja.Prelude


data FcmpLeOp expr = FcmpLeOp
    { _fcmpLeOpLeft :: expr
    , _fcmpLeOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
