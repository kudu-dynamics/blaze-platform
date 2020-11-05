module Binja.Types.MLIL.Op.ModuOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data ModuOp expr = ModuOp
    { _moduOpLeft :: expr
    , _moduOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (ModuOp a)
instance Hashable a => Hashable (ModuOp a)