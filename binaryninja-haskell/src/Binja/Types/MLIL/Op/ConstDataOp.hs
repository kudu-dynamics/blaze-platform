module Binja.Types.MLIL.Op.ConstDataOp where

import Binja.Prelude


data ConstDataOp expr = ConstDataOp
    { _constDataOpConstant :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (ConstDataOp a)
