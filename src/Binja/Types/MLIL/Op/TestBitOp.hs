module Binja.Types.MLIL.Op.TestBitOp where

import Binja.Prelude


data TestBitOp expr = TestBitOp
    { _testBitOpLeft :: expr
    , _testBitOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (TestBitOp a)
instance Hashable a => Hashable (TestBitOp a)