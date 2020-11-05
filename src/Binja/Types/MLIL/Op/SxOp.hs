module Binja.Types.MLIL.Op.SxOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data SxOp expr = SxOp
    { _sxOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (SxOp a)
instance Hashable a => Hashable (SxOp a)