module Binja.Types.MLIL.Op.RetHintOp where

import Binja.Prelude


data RetHintOp expr = RetHintOp
    { _retHintOpDest :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (RetHintOp a)
instance Hashable a => Hashable (RetHintOp a)