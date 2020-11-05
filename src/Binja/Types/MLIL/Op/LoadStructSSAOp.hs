module Binja.Types.MLIL.Op.LoadStructSSAOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data LoadStructSSAOp expr = LoadStructSSAOp
    { _loadStructSSAOpSrc :: expr
    , _loadStructSSAOpOffset :: Int64
    , _loadStructSSAOpSrc_memory :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (LoadStructSSAOp a)
instance Hashable a => Hashable (LoadStructSSAOp a)