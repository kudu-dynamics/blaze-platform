module Binja.Types.MLIL.Op.LoadSSAOp where

import Binja.Prelude


data LoadSSAOp expr = LoadSSAOp
    { _loadSSAOpSrc :: expr
    , _loadSSAOpSrc_memory :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (LoadSSAOp a)
instance Serial m a => Serial m (LoadSSAOp a)
