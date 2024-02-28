module Binja.Types.MLIL.Op.FnegOp where

import Binja.Prelude


newtype FnegOp expr = FnegOp
    { _fnegOpSrc :: expr
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
