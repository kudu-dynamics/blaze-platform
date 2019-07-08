module Hinja.Types.MLIL.Op.LoadSSAOp where

import Hinja.Prelude


data LoadSSAOp expr = LoadSSAOp
    { _loadSSAOpSrc :: expr
    , _loadSSAOpSrc_memory :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
