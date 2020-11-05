module Binja.Types.MLIL.Op.ImportOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data ImportOp expr = ImportOp
    { _importOpConstant :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (ImportOp a)
instance Hashable a => Hashable (ImportOp a)