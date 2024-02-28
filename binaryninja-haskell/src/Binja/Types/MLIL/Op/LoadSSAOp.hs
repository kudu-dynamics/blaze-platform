module Binja.Types.MLIL.Op.LoadSSAOp where

import Binja.Prelude


data LoadSSAOp expr = LoadSSAOp
    { _loadSSAOpSrc :: expr
    , _loadSSAOpSrc_memory :: Int64
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
