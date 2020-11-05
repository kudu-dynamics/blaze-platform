module Binja.Types.MLIL.Op.FnegOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data FnegOp expr = FnegOp
    { _fnegOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (FnegOp a)
instance Hashable a => Hashable (FnegOp a)