module Hinja.Types.MLIL.Op.IntrinsicOp where

import Hinja.Prelude

import Hinja.Types.MLIL.Common (Intrinsic)
import Hinja.Types.Variable (Variable)

data IntrinsicOp expr = IntrinsicOp
    { _intrinsicOpOutput :: [Variable]
    , _intrinsicOpIntrinsic :: Intrinsic
    , _intrinsicOpParams :: [expr]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
