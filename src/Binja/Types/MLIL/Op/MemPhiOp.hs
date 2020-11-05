module Binja.Types.MLIL.Op.MemPhiOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data MemPhiOp expr = MemPhiOp
    { _memPhiOpDest_memory :: Int64
    , _memPhiOpSrc_memory :: [Int64]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (MemPhiOp a)
instance Hashable a => Hashable (MemPhiOp a)