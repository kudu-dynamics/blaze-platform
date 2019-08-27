module Binja.Types.MLIL.Op.SbbOp where

import Binja.Prelude


data SbbOp expr = SbbOp
    { _sbbOpLeft :: expr
    , _sbbOpRight :: expr
    , _sbbOpCarry :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
