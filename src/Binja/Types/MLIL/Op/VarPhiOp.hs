module Binja.Types.MLIL.Op.VarPhiOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)

import Binja.Types.MLIL.Common (SSAVariable)

data VarPhiOp expr = VarPhiOp
    { _varPhiOpDest :: SSAVariable
    , _varPhiOpSrc :: [SSAVariable]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (VarPhiOp a)
instance Hashable a => Hashable (VarPhiOp a)