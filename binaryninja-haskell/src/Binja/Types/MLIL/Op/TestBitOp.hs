module Binja.Types.MLIL.Op.TestBitOp where

import Binja.Prelude


data TestBitOp expr = TestBitOp
    { _testBitOpLeft :: expr
    , _testBitOpRight :: expr
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
