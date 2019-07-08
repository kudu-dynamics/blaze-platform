module Hinja.Types.MLIL.Op.TestBitOp where

import Hinja.Prelude


data TestBitOp expr = TestBitOp
    { _testBitOpLeft :: expr
    , _testBitOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
