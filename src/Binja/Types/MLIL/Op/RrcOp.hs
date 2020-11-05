module Binja.Types.MLIL.Op.RrcOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data RrcOp expr = RrcOp
    { _rrcOpLeft :: expr
    , _rrcOpRight :: expr
    , _rrcOpCarry :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (RrcOp a)
instance Hashable a => Hashable (RrcOp a)