module Binja.Types.MLIL.Op.XorOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data XorOp expr = XorOp
    { _xorOpLeft :: expr
    , _xorOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (XorOp a)
instance Hashable a => Hashable (XorOp a)