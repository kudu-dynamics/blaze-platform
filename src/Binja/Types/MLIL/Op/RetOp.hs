module Binja.Types.MLIL.Op.RetOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data RetOp expr = RetOp
    { _retOpSrc :: [expr]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (RetOp a)
instance Hashable a => Hashable (RetOp a)