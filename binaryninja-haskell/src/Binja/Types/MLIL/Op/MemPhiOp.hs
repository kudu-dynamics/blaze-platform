module Binja.Types.MLIL.Op.MemPhiOp where

import Binja.Prelude


data MemPhiOp expr = MemPhiOp
    { _memPhiOpDest_memory :: Int64
    , _memPhiOpSrc_memory :: [Int64]
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
