module Binja.Types.MLIL.Op.LoadStructSSAOp where

import Binja.Prelude


data LoadStructSSAOp expr = LoadStructSSAOp
    { _loadStructSSAOpSrc :: expr
    , _loadStructSSAOpOffset :: Int64
    , _loadStructSSAOpSrc_memory :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (LoadStructSSAOp a)
instance Serial m a => Serial m (LoadStructSSAOp a)
