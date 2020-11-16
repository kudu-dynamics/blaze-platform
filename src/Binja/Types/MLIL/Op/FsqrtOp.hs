module Binja.Types.MLIL.Op.FsqrtOp where

import Binja.Prelude


data FsqrtOp expr = FsqrtOp
    { _fsqrtOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (FsqrtOp a)
instance Hashable a => Hashable (FsqrtOp a)