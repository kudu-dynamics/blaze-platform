module Binja.Types.MLIL.Op.AsrOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data AsrOp expr = AsrOp
    { _asrOpLeft :: expr
    , _asrOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (AsrOp a)
instance Hashable a => Hashable (AsrOp a)