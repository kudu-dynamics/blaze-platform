module Hinja.Types.MLIL.Op.FcmpLeOp where

import Hinja.Prelude


data FcmpLeOp expr = FcmpLeOp
    { _fcmpLeOpLeft :: expr
    , _fcmpLeOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
