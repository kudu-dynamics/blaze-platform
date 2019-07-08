module Hinja.Types.MLIL.Op.LoadStructSSAOp where

import Hinja.Prelude


data LoadStructSSAOp expr = LoadStructSSAOp
    { _loadStructSSAOpSrc :: expr
    , _loadStructSSAOpOffset :: Int64
    , _loadStructSSAOpSrc_memory :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
