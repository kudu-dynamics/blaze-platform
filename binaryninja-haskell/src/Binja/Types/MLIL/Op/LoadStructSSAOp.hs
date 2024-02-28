module Binja.Types.MLIL.Op.LoadStructSSAOp where

import Binja.Prelude


data LoadStructSSAOp expr = LoadStructSSAOp
    { _loadStructSSAOpSrc :: expr
    , _loadStructSSAOpOffset :: Int64
    , _loadStructSSAOpSrc_memory :: Int64
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
