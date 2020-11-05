module Binja.Types.MLIL.Op.RetHintOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data RetHintOp expr = RetHintOp
    { _retHintOpDest :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (RetHintOp a)
instance Hashable a => Hashable (RetHintOp a)