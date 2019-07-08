module Hinja.Types.MLIL.Op.CallParamSSAOp where

import Hinja.Prelude

import Hinja.Types.MLIL.Common (SSAVariable)

data CallParamSSAOp expr = CallParamSSAOp
    { _callParamSSAOpSrc_memory :: Int64
    , _callParamSSAOpSrc :: [SSAVariable]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
