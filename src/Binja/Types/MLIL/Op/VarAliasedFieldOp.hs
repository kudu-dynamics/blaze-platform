module Binja.Types.MLIL.Op.VarAliasedFieldOp where

import Binja.Prelude

import Binja.Types.MLIL.Common (SSAVariable)

data VarAliasedFieldOp expr = VarAliasedFieldOp
    { _varAliasedFieldOpSrc :: SSAVariable
    , _varAliasedFieldOpOffset :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (VarAliasedFieldOp a)