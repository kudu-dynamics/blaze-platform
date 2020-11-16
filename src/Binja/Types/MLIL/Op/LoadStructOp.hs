module Binja.Types.MLIL.Op.LoadStructOp where

import Binja.Prelude


data LoadStructOp expr = LoadStructOp
    { _loadStructOpSrc :: expr
    , _loadStructOpOffset :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (LoadStructOp a)
instance Hashable a => Hashable (LoadStructOp a)