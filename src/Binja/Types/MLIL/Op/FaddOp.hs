module Binja.Types.MLIL.Op.FaddOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data FaddOp expr = FaddOp
    { _faddOpLeft :: expr
    , _faddOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (FaddOp a)
instance Hashable a => Hashable (FaddOp a)