module Binja.Types.MLIL.Op.AsrOp where

import Binja.Prelude


data AsrOp expr = AsrOp
    { _asrOpLeft :: expr
    , _asrOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (AsrOp a)