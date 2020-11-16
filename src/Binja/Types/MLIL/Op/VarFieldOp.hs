module Binja.Types.MLIL.Op.VarFieldOp where

import Binja.Prelude

import Binja.Types.Variable (Variable)

data VarFieldOp expr = VarFieldOp
    { _varFieldOpSrc :: Variable
    , _varFieldOpOffset :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (VarFieldOp a)
instance Serial m a => Serial m (VarFieldOp a)