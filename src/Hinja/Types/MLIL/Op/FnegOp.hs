module Hinja.Types.MLIL.Op.FnegOp where

import Hinja.Prelude


data FnegOp expr = FnegOp
    { _fnegOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
