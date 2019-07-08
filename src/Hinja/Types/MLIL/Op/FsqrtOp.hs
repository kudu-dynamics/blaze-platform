module Hinja.Types.MLIL.Op.FsqrtOp where

import Hinja.Prelude


data FsqrtOp expr = FsqrtOp
    { _fsqrtOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
