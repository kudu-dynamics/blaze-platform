module Binja.Types.MLIL.Op.FmulOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data FmulOp expr = FmulOp
    { _fmulOpLeft :: expr
    , _fmulOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (FmulOp a)
instance Hashable a => Hashable (FmulOp a)