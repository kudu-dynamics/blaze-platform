module Binja.Types.MLIL.Op.VarAliasedOp where

import Binja.Prelude

import Binja.Types.MLIL.Common (SSAVariable)

data VarAliasedOp expr = VarAliasedOp
    { _varAliasedOpSrc :: SSAVariable
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (VarAliasedOp a)
instance Serial m a => Serial m (VarAliasedOp a)