module Binja.Types.MLIL.Op.GotoOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data GotoOp expr = GotoOp
    { _gotoOpDest :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (GotoOp a)
instance Hashable a => Hashable (GotoOp a)