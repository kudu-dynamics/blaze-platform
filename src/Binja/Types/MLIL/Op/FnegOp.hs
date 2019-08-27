module Binja.Types.MLIL.Op.FnegOp where

import Binja.Prelude


data FnegOp expr = FnegOp
    { _fnegOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
