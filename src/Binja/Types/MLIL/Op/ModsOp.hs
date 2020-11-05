module Binja.Types.MLIL.Op.ModsOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data ModsOp expr = ModsOp
    { _modsOpLeft :: expr
    , _modsOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (ModsOp a)
instance Hashable a => Hashable (ModsOp a)